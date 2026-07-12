# NFL_Schedule_Explorer.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(lubridate)
library(ggplot2)
library(tidyr)
library(httr)
library(jsonlite)
library(here)

# --- Division mapping ---
division_map <- data.frame(
  stringsAsFactors = FALSE,
  Team = c("BUF","MIA","NE","NYJ",
           "BAL","CIN","CLE","PIT",
           "HOU","IND","JAX","TEN",
           "DEN","KC","LAC","LV",
           "DAL","NYG","PHI","WAS",
           "CHI","DET","GB","MIN",
           "ATL","CAR","NO","TB",
           "ARI","LA","SEA","SF"),
  Conference = c(rep("AFC", 16), rep("NFC", 16)),
  Division = c(rep("AFC East", 4), rep("AFC North", 4), rep("AFC South", 4), rep("AFC West", 4),
               rep("NFC East", 4), rep("NFC North", 4), rep("NFC South", 4), rep("NFC West", 4))
)

# Load and prepare data
nfl_data <- read.csv(here::here("nfl_schedule_2026_detailed.csv"), stringsAsFactors = FALSE)

nfl_data <- nfl_data %>%
  mutate(
    parsed_date = mdy(Date),
    parsed_time = parse_date_time(Game_Time, orders = "I:M p", quiet = TRUE),
    hour = hour(parsed_time),
    is_primetime = hour >= 19 | Game_Time == "12:30 PM",
    is_international = !grepl("^America/(New_York|Chicago|Denver|Los_Angeles|Phoenix|Indiana)", TimeZone)
  )

season_start <- min(nfl_data$parsed_date, na.rm = TRUE)
kickoff_time <- as.POSIXct(paste(season_start, "20:20:00"), tz = "America/New_York")

all_teams <- sort(unique(c(nfl_data$Home_Team, nfl_data$Away_Team)))
all_networks <- sort(unique(nfl_data$Network[nfl_data$Network != "TBD"]))

# --- ESPN Standings fetcher ---
fetch_espn_standings <- function(season = 2026) {
  url <- paste0("https://site.api.espn.com/apis/v2/sports/football/nfl/standings?season=", season)
  tryCatch({
    resp <- GET(url, add_headers("User-Agent" = "NFL Schedule Explorer App"))
    if (status_code(resp) != 200) return(NULL)
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

    # Parse the nested structure: children = conferences, children.children = divisions
    results <- list()
    for (conf in data$children) {
      for (div in conf$children) {
        div_name <- div$name
        if (!is.null(div$standings$entries)) {
          entries <- div$standings$entries
          for (i in seq_len(nrow(entries))) {
            team_abbr <- entries$team.abbreviation[i]
            team_name <- entries$team.displayName[i]
            stats <- entries$stats[[i]]
            get_stat <- function(nm) {
              val <- stats$value[stats$name == nm]
              if (length(val) == 0) NA_real_ else val[1]
            }
            results[[length(results) + 1]] <- data.frame(
              Division = div_name,
              Team = team_name,
              Abbr = team_abbr,
              W = get_stat("wins"),
              L = get_stat("losses"),
              T = get_stat("ties"),
              PCT = get_stat("winPercent"),
              PF = get_stat("pointsFor"),
              PA = get_stat("pointsAgainst"),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (length(results) == 0) return(NULL)
    bind_rows(results) %>%
      mutate(DIFF = PF - PA) %>%
      arrange(Division, desc(PCT), desc(DIFF))
  }, error = function(e) {
    message("ESPN standings fetch failed: ", e$message)
    NULL
  })
}

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "NFL 2026 Season Hub"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Schedule Table", tabName = "table", icon = icon("table")),
      menuItem("Standings", tabName = "standings", icon = icon("trophy")),
      menuItem("TV Networks", tabName = "networks", icon = icon("tv")),
      menuItem("Bye Weeks", tabName = "byes", icon = icon("calendar-xmark"))
    ),
    hr(),
    h4("Filters", style = "padding-left: 20px; color: #aaa;"),
    selectInput("team_filter", "Select Team", choices = c("All Teams", all_teams)),
    selectInput("network_filter", "Select Network", choices = c("All Networks", all_networks)),
    br(),
    div(style = "padding: 20px;",
        h5("Countdown to Kickoff", style = "color: #007bff;"),
        h3(textOutput("countdown"), style = "font-weight: bold;")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #1a1a1d !important; }
        .box { background-color: #242526; border-top: 3px solid #007bff; color: white; }
        .box-title { color: white !important; }
        .main-header .logo { font-weight: bold; font-family: 'Arial Black'; }
        .info-box { background: #242526 !important; color: white !important; }
        .dataTables_wrapper { color: white !important; }
        table.dataTable tbody tr { background-color: #242526 !important; color: white !important; }
        table.dataTable thead th { color: white !important; }
        .dataTables_length, .dataTables_filter, .dataTables_info, .dataTables_paginate { color: white !important; }
        .dataTables_paginate .paginate_button { color: white !important; }
        .standings-division { margin-bottom: 20px; }
        .refresh-btn { margin-bottom: 15px; }
      "))
    ),

    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_games", width = 3),
          valueBoxOutput("intl_games", width = 3),
          valueBoxOutput("prime_games", width = 3),
          valueBoxOutput("days_to_start", width = 3)
        ),
        fluidRow(
          box(title = "Upcoming Primetime Matchups", width = 8, status = "primary",
              DT::dataTableOutput("marquee_table")),
          box(title = "Broadcast Distribution", width = 4, status = "info",
              plotOutput("network_plot", height = "300px"))
        ),
        fluidRow(
          box(title = "Games by Day of Week", width = 6, status = "info",
              plotOutput("day_plot", height = "250px")),
          box(title = "International Games", width = 6, status = "primary",
              DT::dataTableOutput("intl_table"))
        )
      ),

      # Table Tab
      tabItem(tabName = "table",
        fluidRow(
          box(title = "Full 2026 Schedule Explorer", width = 12,
              DT::dataTableOutput("full_table"))
        )
      ),

      # Standings Tab
      tabItem(tabName = "standings",
        fluidRow(
          column(12,
            actionButton("refresh_standings", "Refresh Standings",
                         icon = icon("refresh"), class = "btn-primary refresh-btn"),
            span(textOutput("standings_updated"), style = "color: #aaa; margin-left: 15px; line-height: 34px; display: inline-block;")
          )
        ),
        fluidRow(
          box(title = "AFC Standings", width = 6, status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("afc_standings")),
          box(title = "NFC Standings", width = 6, status = "danger", solidHeader = TRUE,
              DT::dataTableOutput("nfc_standings"))
        ),
        fluidRow(
          box(title = "Playoff Picture", width = 12, status = "info", solidHeader = TRUE,
              uiOutput("playoff_picture"))
        )
      ),

      # Networks Tab
      tabItem(tabName = "networks",
        fluidRow(
          box(title = "Games per Network", width = 12, status = "info",
              plotOutput("network_detail_plot", height = "350px"))
        ),
        fluidRow(
          uiOutput("network_cards")
        )
      ),

      # Bye Weeks Tab
      tabItem(tabName = "byes",
        fluidRow(
          box(title = "Bye Week Grid by Division (2026 Season)", width = 12, status = "primary",
              p("Teams sorted by conference and division. Yellow = BYE week.", style = "color: #aaa;"),
              DT::dataTableOutput("bye_table"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    data <- nfl_data
    if (input$team_filter != "All Teams") {
      data <- data %>% filter(Home_Team == input$team_filter | Away_Team == input$team_filter)
    }
    if (input$network_filter != "All Networks") {
      data <- data %>% filter(Network == input$network_filter)
    }
    data
  })

  # Countdown
  output$countdown <- renderText({
    invalidateLater(60000, session)
    diff <- as.numeric(difftime(kickoff_time, Sys.time(), units = "secs"))
    if (diff <= 0) return("Season Underway!")
    days <- floor(diff / 86400)
    hours <- floor((diff %% 86400) / 3600)
    mins <- floor((diff %% 3600) / 60)
    paste0(days, "d ", hours, "h ", mins, "m")
  })

  # Value Boxes
  output$total_games <- renderValueBox({
    valueBox(nrow(nfl_data), "Total Games", icon = icon("football"), color = "blue")
  })

  output$intl_games <- renderValueBox({
    valueBox(sum(nfl_data$is_international), "International", icon = icon("globe-americas"), color = "purple")
  })

  output$prime_games <- renderValueBox({
    valueBox(sum(nfl_data$is_primetime), "Primetime", icon = icon("moon"), color = "yellow")
  })

  output$days_to_start <- renderValueBox({
    days <- as.numeric(season_start - Sys.Date())
    label <- if (days > 0) "Days to Season" else "Season Underway"
    valueBox(max(days, 0), label, icon = icon("calendar-alt"), color = "green")
  })

  # Marquee table: upcoming primetime games
  output$marquee_table <- DT::renderDataTable({
    filtered_data() %>%
      filter(is_primetime, parsed_date >= Sys.Date()) %>%
      arrange(parsed_date) %>%
      head(10) %>%
      select(Week, Day, Date, `Time` = Game_Time, Matchup, Network) %>%
      datatable(options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
  })

  # International games table
  output$intl_table <- DT::renderDataTable({
    nfl_data %>%
      filter(is_international) %>%
      arrange(parsed_date) %>%
      select(Week, Date, Matchup, Stadium, City, Network) %>%
      datatable(options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
  })

  # Full schedule table
  output$full_table <- DT::renderDataTable({
    filtered_data() %>%
      arrange(parsed_date) %>%
      select(Week, Day, Date, `Time` = Game_Time, Matchup, Stadium, City, Network) %>%
      datatable(options = list(pageLength = 16, scrollY = "500px"), rownames = FALSE)
  })

  # Network distribution bar chart
  output$network_plot <- renderPlot({
    counts <- nfl_data %>%
      filter(Network != "TBD") %>%
      count(Network) %>%
      arrange(desc(n))

    ggplot(counts, aes(x = reorder(Network, n), y = n)) +
      geom_col(fill = "#007bff") +
      coord_flip() +
      labs(x = NULL, y = "Games") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  }, bg = "transparent")

  # Day-of-week chart
  output$day_plot <- renderPlot({
    day_order <- c("Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
    counts <- nfl_data %>%
      count(Day) %>%
      mutate(Day = factor(Day, levels = day_order))

    ggplot(counts, aes(x = Day, y = n)) +
      geom_col(fill = "#28a745") +
      labs(x = NULL, y = "Games") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.minor = element_blank()
      )
  }, bg = "transparent")

  # Networks detail plot
  output$network_detail_plot <- renderPlot({
    counts <- nfl_data %>%
      filter(Network != "TBD") %>%
      count(Network) %>%
      arrange(desc(n))

    ggplot(counts, aes(x = reorder(Network, n), y = n)) +
      geom_col(fill = "#007bff", width = 0.7) +
      geom_text(aes(label = n), hjust = -0.2, color = "white", size = 4.5) +
      coord_flip() +
      labs(x = NULL, y = "Total Games Scheduled") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.minor = element_blank()
      )
  }, bg = "transparent")

  # Network Cards
  output$network_cards <- renderUI({
    networks <- nfl_data %>%
      filter(Network != "TBD") %>%
      count(Network) %>%
      arrange(desc(n))

    lapply(seq_len(nrow(networks)), function(i) {
      column(3,
        box(title = networks$Network[i], width = NULL, status = "info", solidHeader = TRUE,
            h2(networks$n[i], style = "text-align: center; font-weight: bold;"),
            p("Games", style = "text-align: center;")
        )
      )
    })
  })

  # --- Bye week grid sorted by division ---
  output$bye_table <- DT::renderDataTable({
    weeks_played <- nfl_data %>%
      select(Week, Home_Team, Away_Team) %>%
      pivot_longer(cols = c(Home_Team, Away_Team), values_to = "Team") %>%
      select(Week, Team) %>%
      distinct()

    all_weeks <- sort(unique(nfl_data$Week))

    bye_grid <- expand.grid(Team = all_teams, Week = all_weeks, stringsAsFactors = FALSE) %>%
      left_join(weeks_played %>% mutate(playing = TRUE), by = c("Team", "Week")) %>%
      mutate(status = ifelse(is.na(playing), "BYE", "")) %>%
      select(Team, Week, status) %>%
      pivot_wider(names_from = Week, values_from = status, names_prefix = "Wk") %>%
      left_join(division_map, by = "Team") %>%
      arrange(Conference, Division, Team) %>%
      select(Division, Team, starts_with("Wk"))

    datatable(bye_grid,
              options = list(pageLength = 32, dom = 't', scrollX = TRUE,
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = FALSE) %>%
      formatStyle("Division", fontWeight = "bold", color = "#007bff") %>%
      formatStyle(names(bye_grid)[-(1:2)],
                  backgroundColor = styleEqual("BYE", "#ffc107"),
                  color = styleEqual("BYE", "#000"))
  })

  # --- Standings ---
  standings_data <- reactiveVal(NULL)
  standings_time <- reactiveVal(NULL)

  # Fetch on first load
  observe({
    standings_data(fetch_espn_standings(2026))
    standings_time(Sys.time())
  }, priority = 100)


  # Refresh button

  observeEvent(input$refresh_standings, {
    standings_data(fetch_espn_standings(2026))
    standings_time(Sys.time())
  })

  output$standings_updated <- renderText({
    ts <- standings_time()
    if (is.null(ts)) return("")
    paste("Last updated:", format(ts, "%b %d, %Y %I:%M %p"))
  })

  # AFC Standings table
  output$afc_standings <- DT::renderDataTable({
    data <- standings_data()
    if (is.null(data)) {
      return(datatable(data.frame(Message = "Standings not available yet. Season may not have started."),
                       options = list(dom = 't'), rownames = FALSE))
    }
    afc <- data %>%
      filter(grepl("^AFC", Division)) %>%
      select(Division, Team, W, L, T, PCT, PF, PA, DIFF) %>%
      mutate(PCT = round(PCT, 3))

    datatable(afc, options = list(pageLength = 16, dom = 't', scrollY = "400px"),
              rownames = FALSE) %>%
      formatStyle("Division", fontWeight = "bold") %>%
      formatStyle("DIFF",
                  color = styleInterval(c(-1, 1), c("#dc3545", "white", "#28a745")))
  })

  # NFC Standings table
  output$nfc_standings <- DT::renderDataTable({
    data <- standings_data()
    if (is.null(data)) {
      return(datatable(data.frame(Message = "Standings not available yet. Season may not have started."),
                       options = list(dom = 't'), rownames = FALSE))
    }
    nfc <- data %>%
      filter(grepl("^NFC", Division)) %>%
      select(Division, Team, W, L, T, PCT, PF, PA, DIFF) %>%
      mutate(PCT = round(PCT, 3))

    datatable(nfc, options = list(pageLength = 16, dom = 't', scrollY = "400px"),
              rownames = FALSE) %>%
      formatStyle("Division", fontWeight = "bold") %>%
      formatStyle("DIFF",
                  color = styleInterval(c(-1, 1), c("#dc3545", "white", "#28a745")))
  })

  # Playoff picture
  output$playoff_picture <- renderUI({
    data <- standings_data()
    if (is.null(data)) {
      return(p("Playoff picture will be available once the season starts and standings data is available.",
               style = "color: #aaa; font-style: italic; padding: 20px;"))
    }

    # Top team per division = division winner; next 3 best records = wild cards
    build_seeds <- function(conf_prefix) {
      conf <- data %>% filter(grepl(paste0("^", conf_prefix), Division))

      # Division winners (rank 1 in each division by PCT)
      div_winners <- conf %>%
        group_by(Division) %>%
        slice_max(PCT, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        arrange(desc(PCT), desc(DIFF))

      # Wild cards: everyone else, top 3
      wild_cards <- conf %>%
        anti_join(div_winners, by = "Abbr") %>%
        arrange(desc(PCT), desc(DIFF)) %>%
        head(3)

      seeds <- bind_rows(
        div_winners %>% mutate(Seed = row_number(), Type = "Division Winner"),
        wild_cards %>% mutate(Seed = 5:7, Type = "Wild Card")
      )
      seeds
    }

    afc_seeds <- build_seeds("AFC")
    nfc_seeds <- build_seeds("NFC")

    format_seed_line <- function(seed_df) {
      lapply(seq_len(nrow(seed_df)), function(i) {
        row <- seed_df[i, ]
        badge_color <- if (row$Seed <= 4) "#28a745" else "#ffc107"
        tags$div(style = "padding: 5px 0; border-bottom: 1px solid #333;",
          span(style = paste0("background:", badge_color, "; color: #000; padding: 2px 8px; border-radius: 3px; font-weight: bold; margin-right: 10px;"),
               paste0("#", row$Seed)),
          strong(row$Team),
          span(style = "color: #aaa; margin-left: 10px;",
               paste0("(", row$W, "-", row$L, ") ", row$Type))
        )
      })
    }

    fluidRow(
      column(6,
        h4("AFC Playoff Seeds", style = "color: #007bff;"),
        format_seed_line(afc_seeds)
      ),
      column(6,
        h4("NFC Playoff Seeds", style = "color: #dc3545;"),
        format_seed_line(nfc_seeds)
      )
    )
  })
}

shinyApp(ui = ui, server = server)
