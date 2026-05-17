# app.R

# 1. LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(DT)
library(here)
library(riem)
library(shinycssloaders)


# 2. LOAD SCHEDULE DATA ----
tryCatch({
  # Load the detailed NFL schedule
  nfl_schedule <- read.csv(here::here("nfl_schedule_2026_detailed.csv"))
  
  # Get unique stadiums for dropdown
  stadium_choices <- unique(nfl_schedule$Stadium)
  stadium_choices <- setNames(stadium_choices, stadium_choices)
  
  # Get unique teams
  all_teams <- sort(unique(c(nfl_schedule$Home_Team, nfl_schedule$Away_Team)))
  
  # Prepare schedule data with proper date/time handling
  schedule_data <- nfl_schedule %>%
    mutate(game_id = row_number()) %>%
    filter(Game_Time != "TBD" & !is.na(TimeZone) & TimeZone != "") %>%
    mutate(
      game_datetime_et = mdy_hm(paste(Date, Game_Time), tz = "America/New_York")
    ) %>%
    filter(!is.na(game_datetime_et))

  # Convert to local timezone per group (much faster than rowwise)
  schedule_data <- bind_rows(lapply(split(schedule_data, schedule_data$TimeZone), function(grp) {
    tz <- grp$TimeZone[1]
    grp$game_datetime <- with_tz(grp$game_datetime_et, tzone = tz)
    grp$game_label <- paste0(
      format(grp$game_datetime, "%b %d"), " - ",
      grp$Away_Team, " @ ", grp$Home_Team, " (",
      format(grp$game_datetime, "%I:%M %p %Z"), ")"
    )
    grp
  })) %>%
    arrange(game_datetime)
  
}, error = function(e) {
  stop(paste("Error processing schedule data:", conditionMessage(e)))
})

# 3. WEATHER ASSESSMENT FUNCTIONS ----

# Parses NWS wind strings like "15 mph", "10 to 20 mph", "Calm" → numeric mph
parse_wind_mph <- function(wind_str) {
  if (is.null(wind_str) || is.na(wind_str) || wind_str == "" ||
      tolower(trimws(wind_str)) == "calm") return(0)
  num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", strsplit(wind_str, " to ")[[1]][1])))
  if (is.na(num)) return(0)
  num
}

# Function to calculate weather severity index
calculate_weather_index <- function(temp, wind_speed, precip_chance, forecast_text) {
  # Initialize scores
  temp_score <- 0
  wind_score <- 0
  precip_score <- 0
  severe_score <- 0
  
  # Handle NULL/NA inputs
  if (is.null(temp)) temp <- NA
  if (is.null(wind_speed)) wind_speed <- "0 mph"
  if (is.null(precip_chance)) precip_chance <- NA
  if (is.null(forecast_text)) forecast_text <- ""
  
  # Temperature impact (below 20°F or above 95°F is concerning)
  if (!is.na(temp)) {
    if (temp < 20) temp_score <- 2  # Very cold
    else if (temp < 32) temp_score <- 1  # Cold/freezing
    else if (temp > 95) temp_score <- 2  # Very hot
    else if (temp > 85) temp_score <- 1  # Hot
  }
  
  # Wind impact
  wind_num <- parse_wind_mph(wind_speed)
  if (!is.na(wind_num)) {
    if (wind_num >= 25) wind_score <- 3
    else if (wind_num >= 20) wind_score <- 2
    else if (wind_num >= 15) wind_score <- 1
  }
  
  # Precipitation impact
  if (!is.na(precip_chance)) {
    if (precip_chance >= 70) precip_score <- 2  # High chance
    else if (precip_chance >= 50) precip_score <- 1  # Moderate chance
  }
  
  # Check for severe weather keywords
  forecast_lower <- tolower(forecast_text)
  severe_keywords <- c("thunder", "lightning", "tornado", "severe", "blizzard", "ice storm", 
                       "freezing rain", "heavy snow", "heavy rain", "flood")
  
  if (forecast_lower != "" && any(sapply(severe_keywords, function(x) grepl(x, forecast_lower)))) {
    severe_score <- 3
  } else if (grepl("snow|rain|storm", forecast_lower)) {
    severe_score <- 1
  }
  
  # Calculate total score
  total_score <- temp_score + wind_score + precip_score + severe_score
  
  # Determine status
  if (total_score >= 6) {
    return(list(status = "RED", 
                level = "Severe Impact",
                color = "#dc3545",
                icon = "⚠️",
                description = "High probability of game impact/delay"))
  } else if (total_score >= 3) {
    return(list(status = "YELLOW", 
                level = "Moderate Impact",
                color = "#ffc107",
                icon = "⚡",
                description = "Possible weather-related issues"))
  } else {
    return(list(status = "GREEN", 
                level = "Good Conditions",
                color = "#28a745",
                icon = "✓",
                description = "No significant weather concerns"))
  }
}

# Function to get detailed weather impact factors
get_impact_factors <- function(temp, wind_speed, precip_chance, forecast_text) {
  factors <- list()
  
  # Temperature factors
  if (!is.na(temp)) {
    if (temp < 20) factors$temperature <- "Extreme cold - player safety concern"
    else if (temp < 32) factors$temperature <- "Freezing conditions - ball handling affected"
    else if (temp > 95) factors$temperature <- "Extreme heat - hydration critical"
    else if (temp > 85) factors$temperature <- "Hot conditions - increased fatigue risk"
  }
  
  # Wind factors
  wind_num <- parse_wind_mph(wind_speed)
  if (wind_num >= 25) factors$wind <- "High winds - severe kicking/passing impact"
  else if (wind_num >= 20) factors$wind <- "Strong winds - moderate kicking/passing impact"
  else if (wind_num >= 15) factors$wind <- "Moderate winds - some kicking impact"
  
  # Precipitation factors
  if (!is.na(precip_chance) && precip_chance > 0) {
    if (precip_chance >= 70) factors$precipitation <- paste0("High chance (", precip_chance, "%) - field conditions likely affected")
    else if (precip_chance >= 50) factors$precipitation <- paste0("Moderate chance (", precip_chance, "%) - monitor conditions")
    else if (precip_chance >= 30) factors$precipitation <- paste0("Low chance (", precip_chance, "%) - minimal impact expected")
  }
  
  # Severe weather check
  forecast_lower <- tolower(forecast_text)
  if (grepl("thunder|lightning", forecast_lower)) factors$severe <- "⚡ Lightning risk - potential game delay"
  if (grepl("snow", forecast_lower)) factors$snow <- "❄️ Snow conditions - visibility and footing affected"
  if (grepl("ice|freezing rain", forecast_lower)) factors$ice <- "🧊 Ice conditions - dangerous playing surface"
  if (grepl("heavy rain", forecast_lower)) factors$rain <- "🌧️ Heavy rain - poor visibility and ball control"
  
  return(factors)
}

# NEW: Function to calculate Kicking Conditions Score
calculate_kicking_score <- function(wind_speed, precip_chance, forecast_text) {
  score <- 10
  issues <- c()
  
  # Wind is the most critical factor
  wind_num <- parse_wind_mph(wind_speed)
  if (wind_num >= 25) { score <- score - 7; issues <- c(issues, "Severe wind impact") }
  else if (wind_num >= 20) { score <- score - 5; issues <- c(issues, "Strong, unpredictable winds") }
  else if (wind_num >= 15) { score <- score - 3; issues <- c(issues, "Moderate wind influence") }
  
  # Precipitation impact
  if (!is.na(precip_chance) && precip_chance >= 50) {
    score <- score - 2
    issues <- c(issues, "Slippery ball/field")
  }
  
  # Severe weather (snow/ice)
  if (grepl("snow|ice|freezing rain", tolower(forecast_text))) {
    score <- score - 3
    issues <- c(issues, "Poor footing and visibility")
  }
  
  if (score < 1) score <- 1
  return(list(score = score, issues = if(length(issues) > 0) paste(issues, collapse = ", ") else "None"))
}

# NEW: Function to calculate Passing Conditions Score
calculate_passing_score <- function(temp, wind_speed, precip_chance, forecast_text) {
  score <- 10
  issues <- c()
  
  # Temperature (grip issues in cold)
  if (!is.na(temp)) {
    if (temp < 25) { score <- score - 3; issues <- c(issues, "Extreme cold affects grip") }
    else if (temp < 40) { score <- score - 1; issues <- c(issues, "Cold may reduce ball feel") }
  }
  
  # Wind (affects ball trajectory)
  wind_num <- parse_wind_mph(wind_speed)
  if (wind_num >= 20) { score <- score - 4; issues <- c(issues, "Deep passes highly affected") }
  else if (wind_num >= 15) { score <- score - 2; issues <- c(issues, "Wind will alter trajectory") }
  
  # Precipitation (grip and visibility)
  if (!is.na(precip_chance)) {
    if (precip_chance >= 70) { score <- score - 3; issues <- c(issues, "High chance of rain/snow") }
    else if (precip_chance >= 50) { score <- score - 2; issues <- c(issues, "Moderate chance of precipitation") }
  }
  
  # Severe weather keywords
  if (grepl("heavy rain|heavy snow|blizzard", tolower(forecast_text))) {
    score <- score - 2 # Additional penalty
    issues <- c(issues, "Poor visibility likely")
  }
  
  if (score < 1) score <- 1
  return(list(score = score, issues = if(length(issues) > 0) paste(issues, collapse = ", ") else "None"))
}

# Rush Advantage: 0 = no weather advantage for running; 10 = extreme run-game weather
calculate_rushing_score <- function(temp, precip_chance, forecast_text) {
  score <- 0
  issues <- c()

  if (!is.na(temp)) {
    if (temp < 20)      { score <- score + 3; issues <- c(issues, "Extreme cold limits passing game") }
    else if (temp < 32) { score <- score + 2; issues <- c(issues, "Freezing — ball control favors run") }
    else if (temp < 40) { score <- score + 1; issues <- c(issues, "Cold conditions slight run lean") }
  }

  if (!is.na(precip_chance)) {
    if (precip_chance >= 70)      { score <- score + 3; issues <- c(issues, "Heavy precip — run game elevated") }
    else if (precip_chance >= 50) { score <- score + 2; issues <- c(issues, "Rain likely — ball security concern") }
    else if (precip_chance >= 30) { score <- score + 1; issues <- c(issues, "Some precip — slight run lean") }
  }

  forecast_lower <- tolower(forecast_text)
  if (grepl("blizzard|heavy snow", forecast_lower)) {
    score <- score + 4; issues <- c(issues, "Blizzard — classic run-game weather")
  } else if (grepl("snow", forecast_lower)) {
    score <- score + 2; issues <- c(issues, "Snow — run game typically elevated")
  }

  if (score > 10) score <- 10
  return(list(score = score, issues = if (length(issues) > 0) paste(issues, collapse = "; ") else "No significant run-game advantage"))
}

# 4. API HELPER FUNCTIONS ----

# 10-minute cache keyed on "lat,lon_d" or "lat,lon_h" — shared across sessions on the same R process
.nws_cache <- new.env(parent = emptyenv())

get_nws_forecast <- function(lat, lon, hourly = FALSE) {
  if (length(lat) != 1 || length(lon) != 1 || is.na(lat) || is.na(lon)) {
    return(data.frame(Status = "Invalid or missing stadium coordinates provided."))
  }
  # NWS API only covers the US/territories (roughly lat 18-72, lon -180 to -60)
  if (lat < 17 || lat > 72 || lon < -180 || lon > -60) {
    return(data.frame(Status = "Location is outside NWS coverage (international venue)."))
  }

  cache_key <- paste0(round(lat, 3), ",", round(lon, 3), "_", if (hourly) "h" else "d")
  cached <- .nws_cache[[cache_key]]
  if (!is.null(cached) && as.numeric(difftime(Sys.time(), cached$time, units = "mins")) < 10) {
    return(cached$data)
  }

  points_url <- paste0("https://api.weather.gov/points/", lat, ",", lon)
  user_agent_header <- add_headers("User-Agent" = "NFL Weather App (RodneyJCuevas@gmail.com)")

  tryCatch({
    points_response <- GET(points_url, user_agent_header)
    stop_for_status(points_response, "get gridpoint metadata")
    points_data <- fromJSON(content(points_response, "text", encoding = "UTF-8"), flatten = TRUE)
    forecast_url <- if (hourly) points_data$properties$forecastHourly else points_data$properties$forecast
    if (is.null(forecast_url)) return(data.frame(Status = "Forecast URL not found for this location."))
    forecast_response <- GET(forecast_url, user_agent_header)
    stop_for_status(forecast_response, "get forecast data")
    forecast_data <- fromJSON(content(forecast_response, "text", encoding = "UTF-8"), flatten = TRUE)
    result <- as_tibble(forecast_data$properties$periods)
    # Only cache well-formed responses
    if ("name" %in% names(result) || "startTime" %in% names(result)) {
      .nws_cache[[cache_key]] <- list(time = Sys.time(), data = result)
    }
    result
  }, error = function(e) {
    message(paste("API Error:", e$message))
    return(data.frame(Status = paste("Could not retrieve forecast. API may be down or location is outside the US.")))
  })
}

# Function to get REAL-TIME conditions using the riem package
get_riem_current_conditions <- function(station_code) {
  if (is.na(station_code) || !grepl("^K", station_code)) {
    # riem/NWS only cover US ASOS stations (ICAO prefix "K")
    return(NULL)
  }

  tryCatch({
    obs <- riem_measures(station = station_code, date_start = Sys.Date())
    if (nrow(obs) > 0) return(slice_tail(obs, n = 1))
    return(NULL)
  }, error = function(e) {
    message(paste("riem error for station", station_code, ":", e$message))
    return(NULL)
  })
}

# Converts NWS compass direction string ("SW", "NNE", etc.) to degrees (0-359)
wind_dir_to_deg <- function(dir_str) {
  dirs <- c(N=0, NNE=22.5, NE=45, ENE=67.5, E=90, ESE=112.5, SE=135, SSE=157.5,
            S=180, SSW=202.5, SW=225, WSW=247.5, W=270, WNW=292.5, NW=315, NNW=337.5)
  dirs[toupper(trimws(dir_str))]
}

# Classifies wind relative to field long axis (Field_Orientation = 0-179 degrees)
get_wind_field_relationship <- function(wind_dir_str, field_orientation) {
  if (is.null(field_orientation) || is.na(field_orientation)) return(NULL)
  wind_deg <- wind_dir_to_deg(wind_dir_str)
  if (is.na(wind_deg)) return(NULL)
  # Smallest angle between wind bearing and either direction of the field axis
  diff1 <- abs(wind_deg - field_orientation) %% 360
  diff1 <- min(diff1, 360 - diff1)
  diff2 <- abs(wind_deg - (field_orientation + 180)) %% 360
  diff2 <- min(diff2, 360 - diff2)
  angle <- min(diff1, diff2)
  if (angle < 30)      list(type = "Along-Field",  desc = "Tailwind/headwind — kicks and deep passes directly affected")
  else if (angle < 60) list(type = "Diagonal",     desc = "Diagonal wind — variable impact on kicks and throws")
  else                 list(type = "Crosswind",    desc = "Crosswind — lateral push on kicks; less pass trajectory impact")
}

# 5. DEFINE USER INTERFACE ----
ui <- dashboardPage(
  dashboardHeader(title = "NFL Stadium Weather Command Center"),
  
  dashboardSidebar(
    h4("Game Selection", style = "padding-left: 15px;"),
    
    # Selection method
    radioButtons("selection_method",
                 "View by:",
                 choices = c("Stadium" = "stadium",
                             "Week" = "week",
                             "Team" = "team",
                             "Date" = "date"),
                 selected = "stadium"),
    
    # Conditional UI based on selection method
    conditionalPanel(
      condition = "input.selection_method == 'stadium'",
      selectInput("selected_stadium",
                  "Select Stadium:",
                  choices = stadium_choices,
                  selected = stadium_choices[1]),
      uiOutput("stadium_games_selector")
    ),
    
    conditionalPanel(
      condition = "input.selection_method == 'week'",
      selectInput("selected_week",
                  "Select Week:",
                  choices = unique(schedule_data$Week),
                  selected = min(schedule_data$Week)),
      uiOutput("week_games_selector")
    ),
    
    conditionalPanel(
      condition = "input.selection_method == 'team'",
      selectInput("selected_team",
                  "Select Team:",
                  choices = all_teams,
                  selected = all_teams[1]),
      uiOutput("team_games_selector")
    ),
    
    conditionalPanel(
      condition = "input.selection_method == 'date'",
      dateInput("selected_date",
                "Select Date:",
                value = Sys.Date(),
                min = Sys.Date(),
                max = max(schedule_data$game_datetime)),
      uiOutput("date_games_selector")
    ),
    
    # Display selected game info
    uiOutput("selected_game_info"),
    
    hr(),
    
    # Legend
    h4("Impact Legend", style = "padding-left: 15px;"),
    tags$div(style = "padding: 0 15px;",
             tags$p(tags$span(style = "color: #28a745; font-weight: bold;", "● GREEN:"), " Good conditions"),
             tags$p(tags$span(style = "color: #ffc107; font-weight: bold;", "● YELLOW:"), " Caution advised"),
             tags$p(tags$span(style = "color: #dc3545; font-weight: bold;", "● RED:"), " Severe impact")
    ),
    
    hr(),
    
    div(style = "padding-left: 15px; font-size: 0.9em;",
        helpText("Data: National Weather Service"),
        br(),
        helpText("For bugs or comments:"),
        p("Rodney Cuevas"),
        p("Meteorologist"),
        p("Developer"),
        tags$a(href="mailto:RodneyJCuevas@gmail.com", "RodneyJCuevas@gmail.com")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .info-box {
          min-height: 90px;
        }
        .weather-alert {
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
          font-weight: bold;
        }
        .weather-green {
          background-color: #d4edda;
          border: 1px solid #c3e6cb;
          color: #155724;
        }
        .weather-yellow {
          background-color: #fff3cd;
          border: 1px solid #ffeeba;
          color: #856404;
        }
        .weather-red {
          background-color: #f8d7da;
          border: 1px solid #f5c6cb;
          color: #721c24;
        }
      "))
    ),
    
    fluidRow(
      column(12,
             h2(textOutput("stadium_city_name")),
             uiOutput("current_weather_alert")
      )
    ),
    
    fluidRow(
      # Key metrics boxes
      valueBoxOutput("temp_box"),
      valueBoxOutput("wind_box"),
      valueBoxOutput("precip_box")
    ),
    
    fluidRow(
      # Impact factors
      box(title = "Game Impact Factors",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          uiOutput("impact_factors"))
    ),
    
    tabsetPanel(type = "tabs",
                tabPanel("7-Day Outlook",
                         br(),
                         fluidRow(
                           box(width = 12,
                               title = "Extended Forecast with Game Impact Assessment",
                               status = "primary",
                               solidHeader = TRUE,
                               DT::dataTableOutput("daily_forecast_enhanced") %>% withSpinner(type = 6, color = "#004085"))
                         )
                ),
                
                tabPanel("Hourly Detail",
                         br(),
                         fluidRow(
                           box(width = 12,
                               title = "Hourly Forecast (Next 24-48 Hours)",
                               status = "info",
                               solidHeader = TRUE,
                               DT::dataTableOutput("hourly_forecast_enhanced") %>% withSpinner(type = 6, color = "#004085"))
                         )
                ),
                
                tabPanel("Game Analysis",
                         br(),
                         fluidRow(
                           box(width = 12,
                               title = "Selected Game Weather Analysis",
                               status = "success",
                               solidHeader = TRUE,
                               uiOutput("gameday_analysis") %>% withSpinner(type = 6, color = "#004085"))
                         )
                ),
                
                tabPanel("Week Overview",
                         br(),
                         fluidRow(
                           box(width = 12,
                               title = "All Games This Week - Weather Status",
                               status = "info",
                               solidHeader = TRUE,
                               # --- THIS CHECKBOX IS NEW ---
                               checkboxInput("hide_domes", "Hide games in domes", value = FALSE),
                               hr(),
                               # --- THE SPINNER IS ADDED TO THE LINE BELOW ---
                               DT::dataTableOutput("week_overview") %>% withSpinner(type = 6, color = "#004085"))
                         )
                )
    )
  )
)

# 6. DEFINE SERVER LOGIC ----
server <- function(input, output, session) {
  
  
  # Reactive to determine the current NFL week
  current_nfl_week <- reactive({
    # Find the earliest game that hasn't happened yet using the precise time
    upcoming_game <- schedule_data %>%
      filter(game_datetime >= Sys.time()) %>% # <-- THE FIX IS HERE
      arrange(game_datetime) %>%
      slice(1) # Get the very next game
    
    if (nrow(upcoming_game) > 0) {
      return(upcoming_game$Week)
    } else {
      # If the season is over, default to the last week
      return(max(schedule_data$Week))
    }
  })
  
  # Observer to set the default week to the current week on app start
  observeEvent(current_nfl_week(), {
    updateSelectInput(session, "selected_week", selected = current_nfl_week())
  }, once = TRUE) # `once = TRUE` ensures this only runs once
  
  
  # Reactive: Get current stadium info
  current_stadium_info <- reactive({
    # REQ: This is the fix.
    # This line tells the reactive to STOP and wait until selected_game() is
    # not NULL. This prevents the entire app from trying to render with
    # invalid or multiple data points at startup.
    req(selected_game())
    
    # Once we have a selected game, the logic is simple.
    game <- selected_game()
    return(list(
      Stadium = game$Stadium,
      City = game$City,
      Latitude = game$Latitude,
      Longitude = game$Longitude
    ))
  })
  
  # UI: Stadium games selector
  output$stadium_games_selector <- renderUI({
    req(input$selected_stadium)
    games <- schedule_data %>%
      filter(
        Stadium == input$selected_stadium,
        as.Date(game_datetime) >= Sys.Date() # Filter for upcoming games here
      ) %>%
      arrange(game_datetime)
    
    if (nrow(games) > 0) {
      choices <- setNames(games$game_id, games$game_label)
      selectInput("selected_stadium_game",
                  "Upcoming Games at this Stadium:", # Label changed for clarity
                  choices = choices,
                  selected = choices[1])
    } else {
      p("No upcoming games found", style = "padding: 0 15px; color: #666;")
    }
  })
  
  # UI: Week games selector
  output$week_games_selector <- renderUI({
    req(input$selected_week)
    
    # Get all games for the selected week
    games_for_week <- schedule_data %>%
      filter(Week == as.numeric(input$selected_week))
    
    # Check if the selected week is the same as the current NFL week
    is_current_week <- as.numeric(input$selected_week) == current_nfl_week()
    
    # If it IS the current week, filter out games that have already started.
    if (is_current_week) {
      games_to_display <- games_for_week %>%
        filter(game_datetime >= Sys.time()) # <-- THE FIX IS HERE
    } else {
      # Otherwise, show all games for the selected week (for past/future weeks).
      games_to_display <- games_for_week
    }
    
    # Arrange the final list of games by date
    games_to_display <- games_to_display %>% arrange(game_datetime)
    
    if (nrow(games_to_display) > 0) {
      choices <- setNames(games_to_display$game_id, games_to_display$game_label)
      selectInput("selected_week_game",
                  "Games This Week:",
                  choices = choices,
                  selected = choices[1])
    } else {
      # Show a helpful message if no games are left in the current week
      p("No remaining games this week.", style = "padding: 0 15px; color: #666;")
    }
  })
  
  # UI: Team games selector
  output$team_games_selector <- renderUI({
    req(input$selected_team)
    games <- schedule_data %>%
      filter(
        (Home_Team == input$selected_team | Away_Team == input$selected_team),
        as.Date(game_datetime) >= Sys.Date() # Filter for upcoming games here
      ) %>%
      arrange(game_datetime)
    
    if (nrow(games) > 0) {
      choices <- setNames(games$game_id, games$game_label)
      selectInput("selected_team_game",
                  "Upcoming Team Schedule:", # Label changed for clarity
                  choices = choices,
                  selected = choices[1])
    } else {
      p("No upcoming games found for this team.", style = "padding: 0 15px; color: #666;")
    }
  })
  
  # UI: Date games selector
  output$date_games_selector <- renderUI({
    req(input$selected_date) # <-- ADD THIS LINE
    games <- schedule_data %>%
      filter(as.Date(game_datetime) == input$selected_date) %>%
      arrange(game_datetime)
    
    if (nrow(games) > 0) {
      choices <- setNames(games$game_id, games$game_label)
      selectInput("selected_date_game",
                  "Games on this Date:",
                  choices = choices,
                  selected = choices[1])
    } else {
      p("No games on this date", style = "padding: 0 15px; color: #666;")
    }
  })
  
  # Reactive: Get selected game
  selected_game <- reactive({
    # Use a different variable name to avoid confusion with the data frame column
    selected_id <- NULL 
    
    if (input$selection_method == "stadium" && !is.null(input$selected_stadium_game)) {
      selected_id <- as.numeric(input$selected_stadium_game)
    } else if (input$selection_method == "week" && !is.null(input$selected_week_game)) {
      selected_id <- as.numeric(input$selected_week_game)
    } else if (input$selection_method == "team" && !is.null(input$selected_team_game)) {
      selected_id <- as.numeric(input$selected_team_game)
    } else if (input$selection_method == "date" && !is.null(input$selected_date_game)) {
      selected_id <- as.numeric(input$selected_date_game)
    }
    
    if (!is.null(selected_id)) {
      # Use the new variable name in the filter
      game <- schedule_data %>% filter(game_id == selected_id)
      if (nrow(game) > 0) {
        # THIS IS THE KEY FIX:
        # Ensure only the first row is ever returned. This prevents
        # multi-element vectors from being passed to the UI.
        return(game[1, ]) 
      }
    }
    return(NULL)
  })
  
  # UI: Display selected game info (with improved text color)
  output$selected_game_info <- renderUI({
    game <- selected_game()
    if (!is.null(game)) {
      div(style = "padding: 10px 15px; background: #e8f4f8; border-radius: 5px; margin: 10px 15px;",
          h5("Selected Game:", style = "margin-top: 0; color: #004085;"),
          
          p(style = "margin: 5px 0; font-weight: bold; color: #333;", 
            paste0(game$Away_Team, " @ ", game$Home_Team)),
          
          # --- THIS SECTION IS MODIFIED TO SHOW DOME/SURFACE INFO ---
          p(style = "margin: 5px 0; color: #333;", 
            paste0("Week ", game$Week, " • ", 
                   if(game$Dome) paste(game$Stadium, "(Dome)") else game$Stadium
            )),
          p(style = "margin: 5px 0; font-size: 0.9em; color: #333;",
            paste("Surface:", game$Surface)),
          # --- END MODIFICATION ---
          
          p(style = "margin: 5px 0; color: #333;",
            format(game$game_datetime, "%B %d, %Y - %I:%M %p %Z"))
      )
    }
  })
  
  # Reactive: Get REAL-TIME current conditions using riem
  current_conditions <- reactive({
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    req(game$Station_ICAO)
    get_riem_current_conditions(game$Station_ICAO)
  })

  # Reactive: Daily forecast (skipped for dome games)
  daily_forecast <- reactive({
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    stadium <- current_stadium_info()
    get_nws_forecast(stadium$Latitude, stadium$Longitude, hourly = FALSE)
  })

  # Reactive: Hourly forecast (skipped for dome games)
  hourly_forecast <- reactive({
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    stadium <- current_stadium_info()
    get_nws_forecast(stadium$Latitude, stadium$Longitude, hourly = TRUE)
  })
  
  # Output: Stadium and city name
  output$stadium_city_name <- renderText({
    stadium <- current_stadium_info()
    paste(stadium$Stadium, "-", stadium$City)
  })
  
  # Output: Current weather alert (with LOCAL observation time)
  output$current_weather_alert <- renderUI({
    game <- selected_game()
    req(game)

    if (isTRUE(game$Dome)) {
      return(div(class = "weather-alert weather-green",
                 h3("\U0001f3df️ Controlled Environment"),
                 p("This game is played in a dome or fully enclosed stadium."),
                 p("Weather conditions have no impact on game play.")))
    }

    # Get all necessary data
    conditions <- current_conditions()
    forecast <- daily_forecast()
    
    # --- Priority 1: Use Real-Time Data if Available ---
    if (!is.null(conditions) && "tmpf" %in% names(conditions)) {
      
      index <- calculate_weather_index(
        temp = conditions$tmpf,
        wind_speed = paste(round(conditions$sknt * 1.15078), "mph"),
        precip_chance = forecast$probabilityOfPrecipitation.value[1],
        forecast_text = forecast$shortForecast[1]
      )
      
      alert_class <- switch(index$status,
                            "GREEN" = "weather-green",
                            "YELLOW" = "weather-yellow",
                            "RED" = "weather-red")
      
      # --- THIS IS THE FIX ---
      # Convert the UTC timestamp from riem to the stadium's local timezone
      local_obs_time <- with_tz(conditions$valid, tzone = game$TimeZone)
      
      div(class = paste("weather-alert", alert_class),
          h3(paste(index$icon, "Current Status:", index$level)),
          p(index$description),
          p(strong("Source Station:"), conditions$station),
          # Display the newly converted local time
          p(strong("Observation Time:"), format(local_obs_time, "%I:%M %p %Z"))
      )
      
      # --- Fallback: Use NWS Forecast if No Real-Time Data ---
    } else if ("temperature" %in% names(forecast) && nrow(forecast) > 0) {
      current_forecast <- forecast[1, ]
      index <- calculate_weather_index(
        current_forecast$temperature,
        current_forecast$windSpeed,
        current_forecast$probabilityOfPrecipitation.value,
        current_forecast$shortForecast
      )
      
      alert_class <- switch(index$status,
                            "GREEN" = "weather-green",
                            "YELLOW" = "weather-yellow",
                            "RED" = "weather-red")
      
      div(class = paste("weather-alert", alert_class),
          h3(paste(index$icon, "Forecast Status:", index$level)),
          p(index$description),
          p(strong("Period:"), current_forecast$name),
          p(strong("Conditions:"), current_forecast$shortForecast)
      )
    }
  })
  
  # Output: Temperature box (now using REAL-TIME data with a forecast fallback)
  output$temp_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("Indoor", "Dome Stadium", icon = icon("building"), color = "purple"))
    }
    conditions <- current_conditions()
    forecast <- daily_forecast()

    if (!is.null(conditions) && !is.na(conditions$tmpf)) {
      # --- Priority 1: Use Real-Time Data if available ---
      temp <- conditions$tmpf
      color <- if (temp < 32 || temp > 85) "red" else if (temp < 40 || temp > 75) "yellow" else "green"
      valueBox(
        paste0(temp, "°F"),
        "Current Temperature",
        icon = icon("thermometer-half"),
        color = color
      )
    } else if ("temperature" %in% names(forecast) && nrow(forecast) > 0) {
      # --- Priority 2: Use Forecast Data if real-time fails ---
      temp <- forecast$temperature[1]
      color <- if (temp < 32 || temp > 85) "red" else if (temp < 40 || temp > 75) "yellow" else "green"
      valueBox(
        paste0(temp, "°F"),
        "Forecasted Temp", # The label is changed to be more accurate
        icon = icon("thermometer-half"),
        color = color
      )
    } else {
      # --- Final Fallback if everything fails ---
      valueBox("N/A", "Temperature", icon = icon("thermometer-half"))
    }
  })
  
  # Output: Wind box (now using REAL-TIME data)
  output$wind_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("N/A", "Dome Stadium", icon = icon("wind"), color = "purple"))
    }
    forecast <- daily_forecast()
    if ("windSpeed" %in% names(forecast) && nrow(forecast) > 0) {
      wind_speed_val <- forecast$windSpeed[1]
      wind_dir <- forecast$windDirection[1]

      wind_mph_num <- parse_wind_mph(wind_speed_val)
      color <- if (wind_mph_num >= 20) "red" else if (wind_mph_num >= 15) "yellow" else "green"

      valueBox(
        paste(wind_speed_val, wind_dir),
        "Forecasted Wind",
        icon = icon("wind"),
        color = color
      )
    } else {
      valueBox("N/A", "Current Wind", icon = icon("wind"))
    }
  })
  
  # Output: Precipitation box
  output$precip_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("N/A", "Dome Stadium", icon = icon("cloud-rain"), color = "purple"))
    }
    forecast <- daily_forecast()
    if ("probabilityOfPrecipitation.value" %in% names(forecast) && nrow(forecast) > 0) {
      precip <- forecast$probabilityOfPrecipitation.value[1]
      precip_val <- ifelse(is.na(precip), 0, precip)
      color <- if (precip_val >= 70) "red" else if (precip_val >= 50) "yellow" else "green"
      
      valueBox(
        paste0(precip_val, "%"),
        "Precipitation Chance",
        icon = icon("cloud-rain"),
        color = color
      )
    } else {
      valueBox("0%", "Precipitation Chance", icon = icon("cloud-rain"))
    }
  })
  
  # Output: Impact factors
  output$impact_factors <- renderUI({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(p("No weather impact factors — this game is played in a controlled indoor environment.",
               style = "color: #6f42c1; font-weight: bold;"))
    }
    forecast <- daily_forecast()
    if ("temperature" %in% names(forecast) && nrow(forecast) > 0) {
      current <- forecast[1,]
      factors <- get_impact_factors(
        current$temperature,
        current$windSpeed,
        current$probabilityOfPrecipitation.value,
        current$shortForecast
      )
      
      if (length(factors) > 0) {
        tags$ul(
          lapply(names(factors), function(name) {
            tags$li(strong(paste0(toupper(substring(name, 1, 1)), substring(name, 2), ":")), 
                    factors[[name]])
          })
        )
      } else {
        p("No significant weather factors detected for game impact.", 
          style = "color: green; font-weight: bold;")
      }
    }
  })
  
  # Output: Enhanced daily forecast table
  output$daily_forecast_enhanced <- DT::renderDataTable({
    forecast <- daily_forecast()
    if ("name" %in% names(forecast)) {
      enhanced_forecast <- forecast %>%
        rowwise() %>%
        mutate(
          weather_index = list(calculate_weather_index(
            temperature,
            windSpeed,
            probabilityOfPrecipitation.value,
            shortForecast
          )),
          Status = weather_index$status,
          Impact = weather_index$level
        ) %>%
        ungroup() %>%
        select(
          Period = name,
          Status,
          Impact,
          Temp = temperature,
          `Precip %` = probabilityOfPrecipitation.value,
          Wind = windSpeed,
          Conditions = shortForecast
        ) %>%
        mutate(
          `Precip %` = ifelse(is.na(`Precip %`), 0, `Precip %`),
          Status = paste0('<span style="color: ', 
                          ifelse(Status == "GREEN", "#28a745",
                                 ifelse(Status == "YELLOW", "#ffc107", "#dc3545")),
                          '; font-weight: bold;">● ', Status, '</span>')
        )
      
      DT::datatable(enhanced_forecast,
                    escape = FALSE,
                    options = list(
                      pageLength = 7,
                      dom = 't',
                      ordering = FALSE
                    ),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Impact',
          backgroundColor = DT::styleEqual(
            c('Good Conditions', 'Moderate Impact', 'Severe Impact'),
            c('#d4edda', '#fff3cd', '#f8d7da')
          )
        )
    } else {
      DT::datatable(forecast)
    }
  }, server = FALSE)
  
  # Output: Enhanced hourly forecast table
  output$hourly_forecast_enhanced <- DT::renderDataTable({
    # Requirement: Get the selected game to access its specific timezone
    game <- selected_game()
    req(game) # Ensure a game is selected before proceeding
    stadium_tz <- game$TimeZone
    
    forecast <- hourly_forecast()
    if ("startTime" %in% names(forecast)) {
      enhanced_hourly <- forecast %>%
        head(48) %>%
        mutate(
          # --- THIS IS THE FIX ---
          # Step 1: Parse the full date-time string from the API.
          # lubridate correctly interprets the offset (e.g., -05:00) and stores it as a UTC instant.
          datetime_utc = ymd_hms(startTime),
          
          # Step 2: Convert that UTC instant to the stadium's actual local timezone.
          datetime_local = with_tz(datetime_utc, tzone = stadium_tz),
          
          # Step 3: Format the CORRECTED local time for display in the table.
          Time = format(datetime_local, "%a %I:%M %p")
        ) %>%
        rowwise() %>%
        mutate(
          weather_index = list(calculate_weather_index(
            temperature,
            windSpeed,
            probabilityOfPrecipitation.value,
            shortForecast
          )),
          Status = weather_index$status
        ) %>%
        ungroup() %>%
        select(
          Time, # Use the new, correctly formatted Time column
          Status,
          Temp = temperature,
          `Precip %` = probabilityOfPrecipitation.value,
          Wind = windSpeed,
          Conditions = shortForecast
        ) %>%
        mutate(
          `Precip %` = ifelse(is.na(`Precip %`), 0, `Precip %`),
          Status = paste0('<span style="color: ', 
                          ifelse(Status == "GREEN", "#28a745",
                                 ifelse(Status == "YELLOW", "#ffc107", "#dc3545")),
                          '; font-weight: bold;">● ', Status, '</span>')
        )
      
      DT::datatable(enhanced_hourly,
                    escape = FALSE,
                    options = list(
                      pageLength = 24,
                      scrollY = "400px",
                      scrollCollapse = TRUE
                    ),
                    rownames = FALSE)
    } else {
      DT::datatable(forecast)
    }
  }, server = FALSE)
  
  # Output: Game day specific analysis
  output$gameday_analysis <- renderUI({
    game <- selected_game()
    
    if (is.null(game)) {
      return(div(
        class = "alert alert-info",
        "Please select a game to view detailed weather analysis."
      ))
    }
    
    forecast <- hourly_forecast()
    
    # Game header
    game_header <- div(
      style = "background: #004085; color: white; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
      h3(style = "margin: 0;", 
         paste(game$Away_Team, "@", game$Home_Team)),
      p(style = "margin: 5px 0 0 0;", 
        "Week ", game$Week, " • ", game$Stadium, ", ", game$City),
      p(style = "margin: 5px 0 0 0; font-size: 0.9em;",
        em("Estimated: ", format(game$game_datetime, "%B %d, %Y - %I:%M %p")))
    )
    
    if (isTRUE(game$Dome)) {
      return(tagList(
        game_header,
        div(class = "weather-alert weather-green",
            h4("\U0001f3df️ Controlled Environment"),
            p("This game is played in a dome or fully enclosed stadium."),
            p("No weather analysis available — conditions have no impact on game play."))
      ))
    }

    if (!"startTime" %in% names(forecast)) {
      return(tagList(
        game_header,
        div(class = "alert alert-warning",
            "Hourly forecast data not available. Showing 7-day outlook instead.")
      ))
    }
    
    # Find forecasts around game time
    game_window <- forecast %>%
      mutate(
        datetime = ymd_hms(startTime),
        hours_from_kickoff = as.numeric(difftime(datetime, game$game_datetime, units = "hours"))
      ) %>%
      filter(hours_from_kickoff >= -3 & hours_from_kickoff <= 4)
    
    if (nrow(game_window) == 0) {
      return(tagList(
        game_header,
        div(class = "alert alert-warning",
            "Game time is beyond available hourly forecast range. Please check closer to game day.")
      ))
    }
    
    sections <- list(game_header)
    
    # Pre-game conditions
    pregame <- game_window %>% filter(hours_from_kickoff >= -3 & hours_from_kickoff < 0)
    if (nrow(pregame) > 0) {
      avg_temp <- round(mean(pregame$temperature, na.rm = TRUE))
      max_precip <- max(pregame$probabilityOfPrecipitation.value, na.rm = TRUE)
      
      sections$pregame <- div(
        style = "background: #f8f9fa; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
        h4("Pre-Game Conditions (Gates Open to Kickoff)"),
        p(strong("Average Temperature:"), paste0(avg_temp, "°F")),
        p(strong("Max Precipitation Chance:"), paste0(max_precip, "%")),
        p(strong("Conditions:"), paste(unique(pregame$shortForecast), collapse = ", "))
      )
    }
    
    # Kickoff conditions
    kickoff <- game_window %>% 
      filter(abs(hours_from_kickoff) == min(abs(game_window$hours_from_kickoff))) %>% 
      slice(1)
    
    if (nrow(kickoff) > 0) {
      index <- calculate_weather_index(
        kickoff$temperature,
        kickoff$windSpeed,
        kickoff$probabilityOfPrecipitation.value,
        kickoff$shortForecast
      )
      
      # Calculate all gameplay scores
      k_score <- calculate_kicking_score(kickoff$windSpeed, kickoff$probabilityOfPrecipitation.value, kickoff$shortForecast)
      p_score <- calculate_passing_score(kickoff$temperature, kickoff$windSpeed, kickoff$probabilityOfPrecipitation.value, kickoff$shortForecast)
      r_score <- calculate_rushing_score(kickoff$temperature, kickoff$probabilityOfPrecipitation.value, kickoff$shortForecast)

      # Wind vs. field orientation (only shown when Field_Orientation column exists)
      field_ori <- if ("Field_Orientation" %in% names(game)) game$Field_Orientation else NA
      wind_rel  <- get_wind_field_relationship(kickoff$windDirection, field_ori)

      sections$kickoff <- div(
        style = paste0("background: #f8f9fa; padding: 15px; margin-bottom: 15px; border-radius: 5px; border-left: 4px solid ", index$color, ";"),
        h4("Kickoff Conditions"),
        p(strong("Status:"),
          span(paste(index$icon, index$level),
               style = paste0("color: ", index$color, "; font-weight: bold;"))),
        p(strong("Temperature:"), paste0(kickoff$temperature, "°F")),
        p(strong("Wind:"), paste(kickoff$windSpeed, "from the", kickoff$windDirection)),
        if (!is.null(wind_rel)) p(strong("Wind Angle:"),
                                  span(wind_rel$type, style = "font-weight: bold;"),
                                  paste0(" — ", wind_rel$desc)),
        p(strong("Precipitation:"), paste0(kickoff$probabilityOfPrecipitation.value, "%")),
        p(strong("Conditions:"), kickoff$shortForecast),

        hr(),
        fluidRow(
          column(4, style = "border-right: 1px solid #ddd;",
                 h5("Kicking Score", style = "text-align: center;"),
                 h3(style = paste0("text-align: center; color: ", if (k_score$score < 5) "red" else if (k_score$score < 8) "orange" else "green", ";"),
                    paste0(k_score$score, "/10")),
                 p(style = "text-align: center; font-style: italic;", k_score$issues)
          ),
          column(4, style = "border-right: 1px solid #ddd;",
                 h5("Passing Score", style = "text-align: center;"),
                 h3(style = paste0("text-align: center; color: ", if (p_score$score < 5) "red" else if (p_score$score < 8) "orange" else "green", ";"),
                    paste0(p_score$score, "/10")),
                 p(style = "text-align: center; font-style: italic;", p_score$issues)
          ),
          column(4,
                 h5("Rush Advantage", style = "text-align: center;"),
                 h3(style = paste0("text-align: center; color: ", if (r_score$score >= 7) "red" else if (r_score$score >= 4) "orange" else "green", ";"),
                    paste0(r_score$score, "/10")),
                 p(style = "text-align: center; font-style: italic;", r_score$issues)
          )
        )
      )
    }
    
    # Game time conditions
    gametime <- game_window %>% filter(hours_from_kickoff >= 0 & hours_from_kickoff <= 4)
    if (nrow(gametime) > 0) {
      worst_index <- "GREEN"
      worst_factors <- list()
      
      for (i in 1:nrow(gametime)) {
        idx <- calculate_weather_index(
          gametime$temperature[i],
          gametime$windSpeed[i],
          gametime$probabilityOfPrecipitation.value[i],
          gametime$shortForecast[i]
        )
        if (idx$status == "RED" || (idx$status == "YELLOW" && worst_index == "GREEN")) {
          worst_index <- idx$status
          worst_factors <- get_impact_factors(
            gametime$temperature[i],
            gametime$windSpeed[i],
            gametime$probabilityOfPrecipitation.value[i],
            gametime$shortForecast[i]
          )
        }
      }
      
      sections$gametime <- div(
        style = "background: #f8f9fa; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
        h4("During Game (Through Final Whistle)"),
        p(strong("Temperature Range:"), 
          paste0(min(gametime$temperature), "°F - ", max(gametime$temperature), "°F")),
        p(strong("Max Wind:"),
          gametime$windSpeed[which.max(sapply(gametime$windSpeed, parse_wind_mph))]),
        p(strong("Max Precipitation Chance:"), 
          paste0(max(gametime$probabilityOfPrecipitation.value, na.rm = TRUE), "%")),
        if (length(worst_factors) > 0) {
          div(
            h5("Critical Factors:", style = "color: #dc3545;"),
            tags$ul(
              lapply(worst_factors, function(f) tags$li(f))
            )
          )
        }
      )
    }
    
    tagList(sections)
  })
  
  # Reactive: fetch weather for all games in the selected week (memoized on week only)
  week_weather_data <- reactive({
    req(input$selected_week)

    week_games <- schedule_data %>%
      filter(Week == as.numeric(input$selected_week))

    if (nrow(week_games) == 0) return(NULL)

    week_games %>%
      rowwise() %>%
      mutate(
        is_in_forecast_window = as.Date(game_datetime) >= Sys.Date() &&
          as.Date(game_datetime) <= (Sys.Date() + 7),

        forecast_data = if (Dome) {
          list(NULL)
        } else if (is_in_forecast_window) {
          list(get_nws_forecast(Latitude, Longitude, hourly = FALSE))
        } else {
          list(NULL)
        },

        game_period = if (!is.null(forecast_data) && "startTime" %in% names(forecast_data) && nrow(forecast_data) > 0) {
          game_time_utc <- with_tz(game_datetime, "UTC")
          matching_period <- forecast_data %>%
            mutate(
              start_time_utc = ymd_hms(startTime),
              end_time_utc = ymd_hms(endTime)
            ) %>%
            filter(game_time_utc >= start_time_utc & game_time_utc < end_time_utc) %>%
            slice(1)
          if (nrow(matching_period) > 0) list(matching_period) else list(slice(forecast_data, 1))
        } else {
          list(NULL)
        },

        current_temp = if (!is.null(game_period)) game_period$temperature[1] else NA,
        current_wind = if (!is.null(game_period)) game_period$windSpeed[1] else "N/A",
        current_precip = if (!is.null(game_period)) game_period$probabilityOfPrecipitation.value[1] else NA,
        current_conditions = if (!is.null(game_period)) game_period$shortForecast[1] else "Forecast Unavailable",

        weather_index = if (Dome) {
          list(list(status = "DOME"))
        } else if (!is.null(game_period)) {
          list(calculate_weather_index(current_temp, current_wind, current_precip, current_conditions))
        } else {
          list(list(status = "TBD"))
        },
        Status = weather_index$status
      ) %>%
      ungroup()
  })

  # Output: Week overview table (toggles dome filter without re-fetching weather)
  output$week_overview <- DT::renderDataTable({
    week_data <- week_weather_data()
    if (is.null(week_data)) return(DT::datatable(data.frame(Message = "No games found for this week")))

    display_data <- if (input$hide_domes) {
      week_data %>% filter(Dome == FALSE)
    } else {
      week_data
    }

    if (nrow(display_data) > 0) {
      week_weather <- display_data %>%
        select(
          Matchup = game_label,
          Stadium,
          City,
          Status,
          `Temp (°F)` = current_temp,
          Wind = current_wind,
          `Precip %` = current_precip
        ) %>%
        mutate(
          `Temp (°F)` = ifelse(is.na(`Temp (°F)`), "N/A", `Temp (°F)`),
          `Precip %` = ifelse(is.na(`Precip %`), "N/A", `Precip %`),
          Status = case_when(
            Status == "GREEN"  ~ '<span style="color: #28a745; font-weight: bold;">● GREEN</span>',
            Status == "YELLOW" ~ '<span style="color: #ffc107; font-weight: bold;">● YELLOW</span>',
            Status == "RED"    ~ '<span style="color: #dc3545; font-weight: bold;">● RED</span>',
            Status == "DOME"   ~ '<span style="color: #6f42c1; font-weight: bold;">\U0001f3df️ DOME</span>',
            TRUE               ~ '<span style="color: #6c757d; font-weight: bold;">● TBD</span>'
          )
        )

      DT::datatable(week_weather,
                    escape = FALSE,
                    options = list(
                      pageLength = 20,
                      scrollY = "400px",
                      scrollCollapse = TRUE
                    ),
                    rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No outdoor games for this week"))
    }
  }, server = FALSE)
}

# 7. RUN THE APPLICATION ----
shinyApp(ui = ui, server = server)