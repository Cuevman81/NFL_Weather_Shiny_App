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
    # The calendar date the game is played on IN ITS OWN TIMEZONE. bind_rows
    # collapses the per-group tzone attribute below, after which as.Date() on
    # game_datetime silently falls back to UTC and rolls night games to the next
    # day. Computing it here, while tz is still correct, is the only safe point.
    grp$game_date <- as.Date(format(grp$game_datetime, "%Y-%m-%d"))
    grp$game_label <- paste0(
      format(grp$game_datetime, "%b %d"), " - ",
      grp$Away_Team, " @ ", grp$Home_Team, " (",
      format(grp$game_datetime, "%I:%M %p %Z"), ")"
    )
    grp
  })) %>%
    arrange(game_datetime)

  # First date that still has games. Used as the date picker's default so the
  # app never opens on an empty day during the offseason.
  upcoming <- schedule_data$game_date[schedule_data$game_datetime >= Sys.time()]
  next_game_date <- if (length(upcoming) > 0) min(upcoming) else Sys.Date()

}, error = function(e) {
  stop(paste("Error processing schedule data:", conditionMessage(e)))
})

# 3. WEATHER ASSESSMENT FUNCTIONS ----

# Parses NWS wind strings like "15 mph", "10 to 20 mph", "Calm" → numeric mph
parse_wind_mph <- function(wind_str) {
  if (is.null(wind_str) || length(wind_str) == 0) return(0)
  wind_str <- as.character(wind_str)[1]  # NWS sends strings, ASOS sends numerics
  if (is.na(wind_str) || wind_str == "" ||
      tolower(trimws(wind_str)) == "calm") return(0)
  # NWS ranges like "15 to 25 mph" are scored against the UPPER bound. Every
  # impact threshold asks "could the wind reach X?", and the top of the range is
  # the honest answer — the low end under-scores a gusty day by a full tier.
  # (Display strings still show the full range; only the number changes.)
  parts <- suppressWarnings(as.numeric(gsub("[^0-9]", "", strsplit(wind_str, " to ")[[1]])))
  parts <- parts[!is.na(parts)]
  if (length(parts) == 0) return(0)
  max(parts)
}

# Apparent temperature using the NWS formulas. Wind chill applies at or below
# 50°F with at least 3 mph of wind; heat index applies at or above 80°F and
# needs relative humidity, which only the hourly feed carries. Otherwise the
# air temperature comes back unchanged. Vectorised.
feels_like_f <- function(temp_f, wind_mph, rh = NA) {
  t <- as.numeric(temp_f)
  v <- rep_len(as.numeric(wind_mph), length(t))
  h <- rep_len(as.numeric(rh),       length(t))
  out <- t

  wc <- !is.na(t) & !is.na(v) & t <= 50 & v >= 3
  out[wc] <- 35.74 + 0.6215 * t[wc] - 35.75 * v[wc]^0.16 + 0.4275 * t[wc] * v[wc]^0.16

  hi <- !is.na(t) & !is.na(h) & t >= 80
  if (any(hi)) {
    T <- t[hi]; R <- h[hi]
    x <- -42.379 + 2.04901523 * T + 10.14333127 * R - 0.22475541 * T * R -
         6.83783e-3 * T^2 - 5.481717e-2 * R^2 + 1.22874e-3 * T^2 * R +
         8.5282e-4 * T * R^2 - 1.99e-6 * T^2 * R^2
    # Rothfusz low-humidity and high-humidity adjustments
    a1 <- R < 13 & T <= 112
    x[a1] <- x[a1] - ((13 - R[a1]) / 4) * sqrt((17 - abs(T[a1] - 95)) / 17)
    a2 <- R > 85 & T <= 87
    x[a2] <- x[a2] + ((R[a2] - 85) / 10) * ((87 - T[a2]) / 5)
    out[hi] <- x
  }
  round(out)
}

# Safe aggregations: return NA instead of -Inf/Inf/NaN when everything is missing
safe_max  <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else max(x) }
safe_min  <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else min(x) }
safe_mean <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_real_ else mean(x) }

# Single source of truth for the colored status pill used in every forecast table.
# Vectorized, so it works on a whole column or a single value.
status_badge <- function(status) {
  colors <- c(GREEN = "#28a745", YELLOW = "#ffc107", RED = "#dc3545",
              DOME = "#6f42c1", TBD = "#6c757d")
  icons  <- c(GREEN = "●", YELLOW = "●", RED = "●",
              DOME = "\U0001f3df️", TBD = "●")
  key <- ifelse(status %in% names(colors), status, "TBD")
  paste0('<span style="color: ', colors[key], '; font-weight: bold;">',
         icons[key], ' ', key, '</span>')
}

# Maps a GREEN/YELLOW/RED status to its alert CSS class
weather_alert_class <- function(status) {
  switch(status,
         GREEN = "weather-green", YELLOW = "weather-yellow", RED = "weather-red",
         "weather-green")
}

# Function to calculate weather severity index
calculate_weather_index <- function(temp, wind_speed, precip_chance, forecast_text, feels_like = NA) {
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
  
  # Temperature impact (below 20°F or above 95°F is concerning). Scored on the
  # apparent temperature when the caller supplies one — a 35°F day at 25 mph is
  # a 22°F wind chill, and that is what players and the ball actually feel.
  if (!is.na(feels_like)) temp <- feels_like
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

get_nws_forecast <- function(lat, lon, hourly = FALSE, station = NULL) {
  if (length(lat) != 1 || length(lon) != 1 || is.na(lat) || is.na(lon)) {
    return(data.frame(Status = "Invalid or missing stadium coordinates provided."))
  }
  # The ICAO prefix is the reliable coverage test: a lat/lon box cannot separate
  # Monterrey (25.7N, -100.3) from south Texas, and NWS 404s on Mexican venues.
  # Every US station starts with "K"; international ones do not.
  if (!is.null(station) && !is.na(station) && !grepl("^K", station)) {
    return(data.frame(Status = "Location is outside NWS coverage (international venue)."))
  }
  # Backstop for callers that don't pass a station (lat 18-72, lon -180 to -60)
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
    # A 12s timeout keeps a slow/throttled NWS response from freezing the
    # single-threaded R process (which would hang every other output).
    points_response <- GET(points_url, user_agent_header, timeout(12))
    stop_for_status(points_response, "get gridpoint metadata")
    points_data <- fromJSON(content(points_response, "text", encoding = "UTF-8"), flatten = TRUE)
    forecast_url <- if (hourly) points_data$properties$forecastHourly else points_data$properties$forecast
    if (is.null(forecast_url)) return(data.frame(Status = "Forecast URL not found for this location."))
    forecast_response <- GET(forecast_url, user_agent_header, timeout(12))
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

# Latest ASOS observation, pulled straight from the Iowa Environmental Mesonet.
# The riem package wraps this same CSV service, but it is built on httr2 and sets
# no timeout anywhere in its namespace — a stalled request would freeze the
# single-threaded R process exactly the way the NWS calls used to. Calling the
# endpoint directly is what lets us bound it. The columns we depend on
# (station / valid / tmpf / sknt) are named the same as riem's output.
get_current_observations <- function(station_code) {
  if (is.null(station_code) || is.na(station_code) || !grepl("^K", station_code)) {
    # NWS/ASOS only cover US stations (ICAO prefix "K")
    return(NULL)
  }

  # IEM rate-limits (HTTP 429) and every viewer of the deployed app shares one
  # outbound IP, so observations get the same short cache the forecasts do.
  # ASOS stations report roughly hourly, so 5 minutes costs no freshness.
  cache_key <- paste0("obs_", station_code)
  cached <- .nws_cache[[cache_key]]
  if (!is.null(cached) && as.numeric(difftime(Sys.time(), cached$time, units = "mins")) < 5) {
    return(cached$data)
  }

  tryCatch({
    # Yesterday through tomorrow in UTC, so a late local kickoff (or a station
    # that reports sparsely) still has observations in range.
    from <- Sys.Date() - 1
    to   <- Sys.Date() + 1
    resp <- GET(
      "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py",
      query = list(
        station = station_code, data = "tmpf,sknt,drct",
        year1 = year(from), month1 = month(from), day1 = day(from),
        year2 = year(to),   month2 = month(to),   day2 = day(to),
        tz = "UTC", format = "onlycomma", latlon = "no",
        missing = "M", trace = "T", direct = "no",
        report_type = "3", report_type = "4"
      ),
      timeout(12)
    )
    stop_for_status(resp, "get ASOS observations")

    obs <- read.csv(text = content(resp, "text", encoding = "UTF-8"),
                    stringsAsFactors = FALSE, na.strings = c("M", "", "T"))
    if (nrow(obs) == 0 || !"tmpf" %in% names(obs)) return(NULL)

    obs <- obs[!is.na(obs$tmpf), , drop = FALSE]
    if (nrow(obs) == 0) return(NULL)

    # IEM stamps observations as "YYYY-MM-DD HH:MM" in UTC
    obs$valid <- ymd_hm(obs$valid, tz = "UTC", quiet = TRUE)
    latest <- obs[nrow(obs), , drop = FALSE]
    .nws_cache[[cache_key]] <- list(time = Sys.time(), data = latest)
    latest
  }, error = function(e) {
    message(paste("ASOS error for station", station_code, ":", e$message))
    return(NULL)
  })
}

# Returns the single forecast period covering `target_time`, or NULL if the
# target falls outside the forecast's range. Works on both the hourly feed
# (1-hour periods) and the daily feed (12-hour day/night periods).
find_period_for_time <- function(forecast, target_time) {
  if (is.null(forecast) || !"startTime" %in% names(forecast) ||
      !"endTime" %in% names(forecast) || nrow(forecast) == 0) return(NULL)
  hit <- forecast %>%
    mutate(.start = ymd_hms(startTime, quiet = TRUE),
           .end   = ymd_hms(endTime,   quiet = TRUE)) %>%
    filter(!is.na(.start), !is.na(.end),
           target_time >= .start, target_time < .end) %>%
    slice(1)
  if (nrow(hit) == 0) NULL else hit
}

# ---- ESPN: standings and scoreboard -----------------------------------------
# ESPN's public endpoints need no key, but their WAF answers 403 to ANY custom
# User-Agent — a plain browser string included. Only httr's default UA gets
# through, so these requests deliberately set none. Timeouts are mandatory (see
# CLAUDE.md): a stalled call here would freeze every connected user's session.

# ESPN -> this app's schedule abbreviations. Every other team already matches.
ESPN_ABBR <- c(LAR = "LA", WSH = "WAS")
to_app_abbr <- function(x) { y <- ESPN_ABBR[x]; unname(ifelse(is.na(y), x, y)) }

.espn_get <- function(url, cache_key, ttl_min) {
  cached <- .nws_cache[[cache_key]]
  if (!is.null(cached) &&
      as.numeric(difftime(Sys.time(), cached$time, units = "mins")) < ttl_min) {
    return(cached$data)
  }
  out <- tryCatch({
    resp <- GET(url, timeout(12))
    stop_for_status(resp, "reach ESPN")
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }, error = function(e) {
    message("ESPN error: ", conditionMessage(e))
    NULL
  })
  if (!is.null(out)) .nws_cache[[cache_key]] <- list(time = Sys.time(), data = out)
  out
}

# One row per team. Seed and clinch status are ESPN's own — computed with the
# full NFL tiebreaker procedure, which a win-pct/point-diff sort cannot
# reproduce. Cached 5 minutes.
fetch_espn_standings <- function(season = 2026) {
  d <- .espn_get(
    paste0("https://site.api.espn.com/apis/v2/sports/football/nfl/standings?season=",
           season, "&level=3&seasontype=2"),  # seasontype=2: regular season only — the default mixes in preseason W-L
    paste0("espn_standings_", season), ttl_min = 5)
  if (is.null(d) || !length(d$children)) return(NULL)

  rows <- list()
  for (conf in d$children) for (div in conf$children) for (e in div$standings$entries) {
    st   <- setNames(e$stats, vapply(e$stats, function(s) s$name, ""))
    val  <- function(n) { s <- st[[n]]; if (is.null(s$value)) NA_real_ else as.numeric(s$value) }
    disp <- function(n) { s <- st[[n]]; if (is.null(s$displayValue)) NA_character_ else s$displayValue }
    rows[[length(rows) + 1]] <- data.frame(
      Conference = conf$abbreviation,
      Division   = div$name,
      Team       = e$team$displayName,
      Abbr       = to_app_abbr(e$team$abbreviation),
      W = val("wins"), L = val("losses"), T = val("ties"),
      PCT = val("winPercent"), PF = val("pointsFor"), PA = val("pointsAgainst"),
      DIFF = val("pointDifferential"),
      Div = disp("divisionRecord"), Conf = disp("vs. Conf."),
      Home = disp("Home"), Road = disp("Road"), Streak = disp("streak"),
      Seed = val("playoffSeed"),
      Clinch = disp("clincher"),
      ClinchDesc = if (is.null(st[["clincher"]]$description)) NA_character_
                   else st[["clincher"]]$description,
      stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  bind_rows(rows) %>% arrange(Conference, Division, Seed, desc(PCT), desc(DIFF))
}

# One row per game in a regular-season week: status, score, records. Cached
# 60 seconds so the in-game auto-refresh can never exceed one call a minute.
fetch_espn_scoreboard <- function(week) {
  d <- .espn_get(
    sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?week=%d&seasontype=2",
            as.integer(week)),
    paste0("espn_scoreboard_", week), ttl_min = 1)
  if (is.null(d) || !length(d$events)) return(NULL)

  bind_rows(lapply(d$events, function(ev) {
    cp   <- ev$competitions[[1]]
    side <- function(which) Filter(function(c) c$homeAway == which, cp$competitors)[[1]]
    home <- side("home"); away <- side("away")
    rec  <- function(c) if (length(c$records)) c$records[[1]]$summary else NA_character_
    data.frame(
      Away = to_app_abbr(away$team$abbreviation), Home = to_app_abbr(home$team$abbreviation),
      AwayScore = suppressWarnings(as.integer(away$score)),
      HomeScore = suppressWarnings(as.integer(home$score)),
      AwayRecord = rec(away), HomeRecord = rec(home),
      StatusName = cp$status$type$name, StatusDetail = cp$status$type$detail,
      Completed = isTRUE(cp$status$type$completed),
      stringsAsFactors = FALSE)
  }))
}

# "24-17 Final", "7-3 Q2 8:41", or "" for a game that hasn't kicked off.
score_label <- function(sb_row) {
  if (is.null(sb_row) || nrow(sb_row) == 0 || sb_row$StatusName[1] == "STATUS_SCHEDULED") return("")
  paste0(sb_row$AwayScore[1], "-", sb_row$HomeScore[1], " ",
         if (isTRUE(sb_row$Completed[1])) "Final" else sb_row$StatusDetail[1])
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
  # `title` here is what actually sets the browser tab; a tags$title() in the
  # body head gets overridden by dashboardPage.
  title = "NFL Gameday Weather",
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
                # Open on the next date that actually has games — outside the
                # season "today" is always an empty result.
                value = next_game_date,
                min = Sys.Date(),
                max = max(as.Date(schedule_data$game_datetime))),
      uiOutput("date_games_selector")
    ),
    
    # Display selected game info
    uiOutput("selected_game_info"),

    # Manual refresh for live weather (clears the 10-min NWS cache and re-pulls)
    div(style = "padding: 5px 15px;",
        actionButton("refresh_weather", "Refresh Weather & Scores",
                     icon = icon("sync"), class = "btn-block")),
    div(style = "padding: 0 15px 5px; font-size: 0.8em; color: #aaa;",
        textOutput("weather_last_updated")),

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
        helpText("Weather: National Weather Service"),
        helpText("Observations: Iowa Environmental Mesonet"),
        helpText("Standings & scores: ESPN"),
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
      ")),
      # DataTables inside a tab that is hidden at load render with collapsed
      # column widths / zero scroll height. When any tab becomes visible, tell
      # every DataTable to recalculate its layout so the rows show immediately.
      # The guards matter: this event also fires during page init, before DT's
      # JS has registered $.fn.dataTable. Throwing there aborts Shiny's client
      # message pipeline and every lazily-rendered output stays stuck on its
      # spinner forever.
      tags$script(HTML(
        "$(document).on('shown.bs.tab', function(e) {
           try {
             if ($.fn && $.fn.dataTable && $.fn.dataTable.tables) {
               var t = $.fn.dataTable.tables(true);
               if (t.length) $(t).DataTable().columns.adjust();
             }
           } catch (err) {
             if (window.console) console.warn('DataTable adjust skipped:', err);
           }
           window.dispatchEvent(new Event('resize'));
         });"
      ))
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
    
    tabsetPanel(id = "main_tabs", type = "tabs",
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
                               h4(textOutput("week_overview_label"), style = "margin-top: 0;"),
                               checkboxInput("hide_domes", "Hide games in domes", value = FALSE),
                               hr(),
                               DT::dataTableOutput("week_overview") %>% withSpinner(type = 6, color = "#004085"))
                         )
                ),

                tabPanel("Standings & Scores",
                         br(),
                         fluidRow(
                           box(width = 12,
                               title = "Scoreboard",
                               status = "primary",
                               solidHeader = TRUE,
                               h4(textOutput("scores_caption"), style = "margin-top: 0;"),
                               DT::dataTableOutput("scoreboard_table") %>% withSpinner(type = 6, color = "#004085"))
                         ),
                         fluidRow(
                           box(width = 12,
                               title = "Playoff Picture",
                               status = "success",
                               solidHeader = TRUE,
                               uiOutput("playoff_picture"))
                         ),
                         fluidRow(
                           box(width = 6,
                               title = "AFC Standings",
                               status = "danger",
                               solidHeader = TRUE,
                               DT::dataTableOutput("afc_standings") %>% withSpinner(type = 6, color = "#004085")),
                           box(width = 6,
                               title = "NFC Standings",
                               status = "info",
                               solidHeader = TRUE,
                               DT::dataTableOutput("nfc_standings") %>% withSpinner(type = 6, color = "#004085"))
                         )
                )
    )
  )
)

# 6. DEFINE SERVER LOGIC ----
server <- function(input, output, session) {

  # Manual weather refresh: clears the shared NWS cache and re-triggers every
  # weather reactive that takes a dependency on weather_refresh().
  weather_refresh <- reactiveVal(Sys.time())
  observeEvent(input$refresh_weather, {
    keys <- ls(.nws_cache)
    if (length(keys) > 0) rm(list = keys, envir = .nws_cache)
    weather_refresh(Sys.time())
  })

  output$weather_last_updated <- renderText({
    paste("Data updated", format(weather_refresh(), "%I:%M %p %Z"))
  })

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
      Longitude = game$Longitude,
      Station_ICAO = game$Station_ICAO
    ))
  })
  
  # UI: Stadium games selector
  output$stadium_games_selector <- renderUI({
    req(input$selected_stadium)
    games <- schedule_data %>%
      filter(
        Stadium == input$selected_stadium,
        game_date >= Sys.Date() # Filter for upcoming games here
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
        game_date >= Sys.Date() # Filter for upcoming games here
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
      filter(game_date == input$selected_date) %>%
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
            format(with_tz(game$game_datetime, game$TimeZone), "%B %d, %Y - %I:%M %p %Z"))
      )
    }
  })
  
  # Reactive: Get REAL-TIME current conditions using riem
  current_conditions <- reactive({
    weather_refresh()  # re-fetch when the user clicks Refresh Weather Data
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    req(game$Station_ICAO)
    get_current_observations(game$Station_ICAO)
  })

  # Reactive: Daily forecast (skipped for dome games)
  daily_forecast <- reactive({
    weather_refresh()  # re-fetch when the user clicks Refresh Weather Data
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    stadium <- current_stadium_info()
    get_nws_forecast(stadium$Latitude, stadium$Longitude, hourly = FALSE,
                     station = stadium$Station_ICAO)
  })

  # Reactive: Hourly forecast (skipped for dome games)
  hourly_forecast <- reactive({
    weather_refresh()  # re-fetch when the user clicks Refresh Weather Data
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    stadium <- current_stadium_info()
    get_nws_forecast(stadium$Latitude, stadium$Longitude, hourly = TRUE,
                     station = stadium$Station_ICAO)
  })
  
  # Reactive: the forecast period that actually covers kickoff, or NULL when the
  # game is past / beyond the forecast horizon. Hourly is preferred (1-hour
  # resolution, ~6 days out); the 12-hour daily periods extend the reach.
  kickoff_forecast <- reactive({
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)
    kick <- game$game_datetime

    hit <- find_period_for_time(hourly_forecast(), kick)
    if (!is.null(hit)) return(list(period = hit, resolution = "hourly"))

    hit <- find_period_for_time(daily_forecast(), kick)
    if (!is.null(hit)) return(list(period = hit, resolution = "daily"))

    NULL
  })

  # Single source of truth for the headline alert + value boxes. This app is
  # about GAME weather, so kickoff conditions win whenever the game is inside
  # the forecast horizon; live conditions at the venue are the labeled fallback
  # for games still weeks out.
  display_conditions <- reactive({
    game <- selected_game()
    req(game)
    if (isTRUE(game$Dome)) return(NULL)

    kf <- kickoff_forecast()
    if (!is.null(kf)) {
      p <- kf$period
      return(list(
        scope      = "kickoff",
        prefix     = "Kickoff",
        temp       = p$temperature[1],
        wind       = p$windSpeed[1],
        wind_dir   = p$windDirection[1],
        precip     = p$probabilityOfPrecipitation.value[1],
        conditions = p$shortForecast[1],
        feels_like = feels_like_f(
          p$temperature[1], parse_wind_mph(p$windSpeed[1]),
          if ("relativeHumidity.value" %in% names(p)) p$relativeHumidity.value[1] else NA),
        resolution = kf$resolution
      ))
    }

    forecast <- daily_forecast()
    has_fc   <- "temperature" %in% names(forecast) && nrow(forecast) > 0
    obs      <- current_conditions()
    has_obs  <- !is.null(obs) && "tmpf" %in% names(obs) && !is.na(obs$tmpf)
    if (!has_fc && !has_obs) return(NULL)

    list(
      scope      = "current",
      prefix     = "Current",
      temp       = if (has_obs) obs$tmpf else forecast$temperature[1],
      wind       = if (has_obs) paste(round(obs$sknt * 1.15078), "mph")
                   else if (has_fc) forecast$windSpeed[1] else "N/A",
      wind_dir   = if (has_obs) "" else if (has_fc) forecast$windDirection[1] else "",
      precip     = if (has_fc) forecast$probabilityOfPrecipitation.value[1] else NA,
      conditions = if (has_fc) forecast$shortForecast[1] else NA_character_,
      station    = if (has_obs) obs$station else NA_character_,
      obs_time   = if (has_obs) obs$valid else NULL
    )
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

    dc <- display_conditions()
    if (is.null(dc)) return(NULL)

    fl <- if (is.null(dc$feels_like)) NA else dc$feels_like
    index <- calculate_weather_index(dc$temp, dc$wind, dc$precip, dc$conditions, feels_like = fl)
    alert_class <- weather_alert_class(index$status)

    if (identical(dc$scope, "kickoff")) {
      return(div(class = paste("weather-alert", alert_class),
                 h3(paste(index$icon, "Kickoff Forecast:", index$level)),
                 p(index$description),
                 p(strong("Conditions:"), dc$conditions),
                 if (!is.na(fl) && abs(fl - dc$temp) >= 2)
                   p(strong("Feels like:"), paste0(fl, "°F"),
                     span(style = "color:#666;", if (fl < dc$temp) " (wind chill)" else " (heat index)")),
                 p(strong("Kickoff:"),
                   format(with_tz(game$game_datetime, game$TimeZone),
                          "%a %b %d, %I:%M %p %Z"))))
    }

    # No kickoff forecast available — say why rather than passing off today's
    # weather at the venue as if it were the game's.
    days_out <- as.numeric(difftime(game$game_datetime, Sys.time(), units = "days"))
    note <- if (days_out < 0) {
      "This game has already kicked off or been played — showing conditions at the venue now."
    } else {
      paste0("Kickoff is about ", max(1, round(days_out)),
             " days out, beyond the NWS forecast range. Showing conditions at the venue now — ",
             "check back within a week of the game for a kickoff forecast.")
    }

    div(class = paste("weather-alert", alert_class),
        h3(paste(index$icon, "Current Conditions at Venue:", index$level)),
        p(note),
        if (!is.na(dc$station)) p(strong("Source Station:"), dc$station),
        if (!is.null(dc$obs_time))
          p(strong("Observation Time:"),
            format(with_tz(dc$obs_time, tzone = game$TimeZone), "%I:%M %p %Z"))
    )
  })
  
  # Output: Temperature box (now using REAL-TIME data with a forecast fallback)
  output$temp_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("Indoor", "Dome Stadium", icon = icon("building"), color = "purple"))
    }
    dc <- display_conditions()
    if (is.null(dc) || is.na(dc$temp)) {
      return(valueBox("N/A", "Temperature", icon = icon("thermometer-half")))
    }
    temp <- dc$temp
    color <- if (temp < 32 || temp > 85) "red" else if (temp < 40 || temp > 75) "yellow" else "green"
    fl <- if (is.null(dc$feels_like)) NA else dc$feels_like
    subtitle <- paste(dc$prefix, "Temperature")
    if (!is.na(fl) && abs(fl - temp) >= 2) subtitle <- paste0(subtitle, " · feels like ", fl, "°F")
    valueBox(
      paste0(temp, "°F"),
      subtitle,
      icon = icon("thermometer-half"),
      color = color
    )
  })
  
  # Output: Wind box (now using REAL-TIME data)
  output$wind_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("N/A", "Dome Stadium", icon = icon("wind"), color = "purple"))
    }
    dc <- display_conditions()
    if (is.null(dc) || identical(dc$wind, "N/A")) {
      return(valueBox("N/A", "Wind", icon = icon("wind")))
    }
    wind_mph_num <- parse_wind_mph(dc$wind)
    color <- if (wind_mph_num >= 20) "red" else if (wind_mph_num >= 15) "yellow" else "green"

    valueBox(
      trimws(paste(dc$wind, dc$wind_dir)),
      paste(dc$prefix, "Wind"),
      icon = icon("wind"),
      color = color
    )
  })
  
  # Output: Precipitation box
  output$precip_box <- renderValueBox({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(valueBox("N/A", "Dome Stadium", icon = icon("cloud-rain"), color = "purple"))
    }
    dc <- display_conditions()
    if (is.null(dc)) {
      return(valueBox("N/A", "Precipitation Chance", icon = icon("cloud-rain")))
    }
    precip_val <- ifelse(is.na(dc$precip), 0, dc$precip)
    color <- if (precip_val >= 70) "red" else if (precip_val >= 50) "yellow" else "green"

    valueBox(
      paste0(precip_val, "%"),
      paste(dc$prefix, "Precip Chance"),
      icon = icon("cloud-rain"),
      color = color
    )
  })
  
  # Output: Impact factors
  output$impact_factors <- renderUI({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(p("No weather impact factors — this game is played in a controlled indoor environment.",
               style = "color: #6f42c1; font-weight: bold;"))
    }
    dc <- display_conditions()
    if (is.null(dc)) return(NULL)

    factors <- get_impact_factors(dc$temp, dc$wind, dc$precip, dc$conditions)

    scope_note <- if (identical(dc$scope, "kickoff")) {
      "Based on the forecast period covering kickoff."
    } else {
      "Based on conditions at the venue now — kickoff is outside the forecast range."
    }

    tagList(
      p(em(scope_note), style = "color: #666; font-size: 0.9em; margin-bottom: 10px;"),
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
    )
  })
  
  # Output: Enhanced daily forecast table
  output$daily_forecast_enhanced <- DT::renderDataTable({
    game <- selected_game()
    if (!is.null(game) && isTRUE(game$Dome)) {
      return(DT::datatable(data.frame(Note = "Indoor / dome stadium — weather forecast does not apply."),
                           options = list(dom = "t"), rownames = FALSE, colnames = ""))
    }
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
          Status = status_badge(Status)
        )
      
      DT::datatable(enhanced_forecast,
                    escape = -2,  # only the Status badge (col 2) carries HTML; NWS text stays escaped
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
    input$main_tabs  # re-render when this tab is opened so the DataTable draws in a visible container
    # Requirement: Get the selected game to access its specific timezone
    game <- selected_game()
    req(game) # Ensure a game is selected before proceeding
    if (isTRUE(game$Dome)) {
      return(DT::datatable(data.frame(Note = "Indoor / dome stadium — no hourly forecast."),
                           options = list(dom = "t"), rownames = FALSE, colnames = ""))
    }
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
          Status = status_badge(Status)
        )
      
      DT::datatable(enhanced_hourly,
                    escape = -2,  # only the Status badge (col 2) carries HTML; NWS text stays escaped
                    options = list(
                      pageLength = 12,
                      lengthMenu = c(12, 24, 48)
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
        em("Estimated: ", format(with_tz(game$game_datetime, game$TimeZone), "%B %d, %Y - %I:%M %p %Z")))
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
      avg_temp <- safe_mean(pregame$temperature)
      max_precip <- safe_max(pregame$probabilityOfPrecipitation.value)

      sections$pregame <- div(
        style = "background: #f8f9fa; padding: 15px; margin-bottom: 15px; border-radius: 5px;",
        h4("Pre-Game Conditions (Gates Open to Kickoff)"),
        p(strong("Average Temperature:"), if (is.na(avg_temp)) "N/A" else paste0(round(avg_temp), "°F")),
        p(strong("Max Precipitation Chance:"), if (is.na(max_precip)) "N/A" else paste0(max_precip, "%")),
        p(strong("Conditions:"), paste(unique(pregame$shortForecast), collapse = ", "))
      )
    }
    
    # Kickoff conditions
    kickoff <- game_window %>% 
      filter(abs(hours_from_kickoff) == min(abs(game_window$hours_from_kickoff))) %>% 
      slice(1)
    
    if (nrow(kickoff) > 0) {
      kick_feels <- feels_like_f(
        kickoff$temperature, parse_wind_mph(kickoff$windSpeed),
        if ("relativeHumidity.value" %in% names(kickoff)) kickoff$relativeHumidity.value else NA)
      index <- calculate_weather_index(
        kickoff$temperature,
        kickoff$windSpeed,
        kickoff$probabilityOfPrecipitation.value,
        kickoff$shortForecast,
        feels_like = kick_feels
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
        p(strong("Temperature:"), paste0(kickoff$temperature, "°F"),
          if (!is.na(kick_feels) && abs(kick_feels - kickoff$temperature) >= 2)
            span(style = "color:#666;", paste0(" (feels like ", kick_feels, "°F)"))),
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
          paste0(safe_min(gametime$temperature), "°F - ", safe_max(gametime$temperature), "°F")),
        p(strong("Max Wind:"),
          gametime$windSpeed[which.max(sapply(gametime$windSpeed, parse_wind_mph))]),
        p(strong("Max Precipitation Chance:"),
          {mp <- safe_max(gametime$probabilityOfPrecipitation.value); if (is.na(mp)) "N/A" else paste0(mp, "%")}),
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
  # Which week the overview tab describes. In Week mode that's the sidebar
  # dropdown; in Stadium/Team/Date mode it follows the game you actually have
  # selected, so the tab can't silently show a different week than the rest of
  # the dashboard.
  overview_week <- reactive({
    if (identical(input$selection_method, "week")) {
      req(input$selected_week)
      as.numeric(input$selected_week)
    } else {
      game <- selected_game()
      req(game)
      as.numeric(game$Week)
    }
  })

  output$week_overview_label <- renderText({
    paste("Week", overview_week())
  })

  week_weather_data <- reactive({
    weather_refresh()  # re-fetch when the user clicks Refresh Weather Data
    sel_week <- overview_week()

    week_games <- schedule_data %>%
      filter(Week == sel_week)

    if (nrow(week_games) == 0) return(NULL)

    week_games %>%
      rowwise() %>%
      mutate(
        is_in_forecast_window = game_date >= Sys.Date() &&
          game_date <= (Sys.Date() + 7),

        forecast_data = if (Dome) {
          list(NULL)
        } else if (is_in_forecast_window) {
          list(get_nws_forecast(Latitude, Longitude, hourly = FALSE, station = Station_ICAO))
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
    # This table fetches a forecast for every stadium playing that week (up to 16
    # sequential NWS round trips). Gate it on the tab actually being open so a
    # page load never pays that cost, and so the fetch re-runs in a visible
    # container once the user gets here.
    req(identical(input$main_tabs, "Week Overview"))
    week_data <- week_weather_data()
    if (is.null(week_data)) return(DT::datatable(data.frame(Message = "No games found for this week")))

    display_data <- if (input$hide_domes) {
      week_data %>% filter(Dome == FALSE)
    } else {
      week_data
    }

    if (nrow(display_data) > 0) {
      # Once a game kicks off, ESPN's score/status lands beside its weather row
      sb <- scoreboard_data()
      score_for <- function(away, home) {
        if (is.null(sb)) return("")
        score_label(sb[sb$Away == away & sb$Home == home, , drop = FALSE])
      }
      week_weather <- display_data %>%
        mutate(Score = unname(mapply(score_for, Away_Team, Home_Team))) %>%
        select(
          Matchup = game_label,
          Stadium,
          City,
          Status,
          Score,
          `Temp (°F)` = current_temp,
          Wind = current_wind,
          `Precip %` = current_precip
        ) %>%
        mutate(
          `Temp (°F)` = ifelse(is.na(`Temp (°F)`), "N/A", `Temp (°F)`),
          `Precip %` = ifelse(is.na(`Precip %`), "N/A", `Precip %`),
          Status = status_badge(Status)
        )

      DT::datatable(week_weather,
                    escape = -4,  # only the Status badge (col 4) carries HTML; everything else stays escaped
                    options = list(
                      pageLength = 16,
                      lengthMenu = c(16, 32)
                    ),
                    rownames = FALSE)
    } else {
      DT::datatable(data.frame(Message = "No outdoor games for this week"))
    }
  }, server = FALSE)

  # ---- Standings & Scores ---------------------------------------------------
  # Scoreboard for the week the dashboard is showing. While that tab is open AND
  # a game is in progress, re-poll every ~65s (the fetch is cached 60s, so this
  # can't exceed one ESPN call a minute). A scheduled or finished slate doesn't
  # change, so otherwise there is no polling at all.
  scoreboard_data <- reactive({
    weather_refresh()
    sb <- fetch_espn_scoreboard(overview_week())
    if (identical(input$main_tabs, "Standings & Scores") &&
        !is.null(sb) && any(sb$StatusName == "STATUS_IN_PROGRESS")) {
      invalidateLater(65000, session)
    }
    sb
  })

  standings_data <- reactive({
    weather_refresh()
    req(identical(input$main_tabs, "Standings & Scores"))
    fetch_espn_standings(2026)
  })

  output$scores_caption <- renderText({
    wk <- overview_week()
    sb <- scoreboard_data()
    if (is.null(sb)) return(paste("Week", wk))
    live <- sum(sb$StatusName == "STATUS_IN_PROGRESS")
    done <- sum(sb$Completed)
    paste0("Week ", wk, " — ", done, " final · ", live, " in progress · ",
           nrow(sb) - done - live, " upcoming",
           if (live > 0) "   (auto-refreshing every minute)" else "")
  })

  output$scoreboard_table <- DT::renderDataTable({
    req(identical(input$main_tabs, "Standings & Scores"))
    sb <- scoreboard_data()
    if (is.null(sb)) {
      return(DT::datatable(data.frame(Note = "Scores unavailable — ESPN did not respond."),
                           options = list(dom = "t"), rownames = FALSE, colnames = ""))
    }
    sb %>%
      mutate(
        Matchup = paste0(Away, " @ ", Home),
        Score   = vapply(seq_len(n()), function(i) score_label(sb[i, , drop = FALSE]), ""),
        Status  = ifelse(Completed, "Final",
                    ifelse(StatusName == "STATUS_IN_PROGRESS", paste("LIVE —", StatusDetail), StatusDetail)),
        Records = paste0(Away, " ", AwayRecord, "  ·  ", Home, " ", HomeRecord)
      ) %>%
      select(Matchup, Status, Score, Records) %>%
      DT::datatable(options = list(pageLength = 16, dom = "t", ordering = FALSE), rownames = FALSE) %>%
      DT::formatStyle("Status", fontWeight = DT::styleEqual("Final", "bold"))
  }, server = FALSE)

  standings_table <- function(conf_abbr) {
    DT::renderDataTable({
      req(identical(input$main_tabs, "Standings & Scores"))
      d <- standings_data()
      if (is.null(d)) {
        return(DT::datatable(data.frame(Note = "Standings unavailable — ESPN did not respond."),
                             options = list(dom = "t"), rownames = FALSE, colnames = ""))
      }
      d %>%
        filter(Conference == conf_abbr) %>%
        mutate(
          Seed   = ifelse(!is.na(Seed) & Seed >= 1 & Seed <= 7, paste0("#", Seed), ""),  # ESPN reports 0 before any games
          Clinch = ifelse(is.na(Clinch), "", Clinch),
          PCT    = sprintf("%.3f", PCT)
        ) %>%
        select(Division, Seed, Team, W, L, T, PCT, PF, PA, DIFF,
               Div, Conf, Home, Road, Streak, Clinch) %>%
        DT::datatable(options = list(pageLength = 16, dom = "t", ordering = FALSE, scrollX = TRUE),
                      rownames = FALSE) %>%
        DT::formatStyle("DIFF", color = DT::styleInterval(c(-1, 0), c("#dc3545", "#333", "#28a745")))
    }, server = FALSE)
  }
  output$afc_standings <- standings_table("AFC")
  output$nfc_standings <- standings_table("NFC")

  # Seeds 1-7 per conference straight from ESPN's playoffSeed, which applies the
  # full NFL tiebreaker procedure. Clinch codes appear as the season plays out.
  output$playoff_picture <- renderUI({
    req(identical(input$main_tabs, "Standings & Scores"))
    d <- standings_data()
    if (is.null(d)) return(NULL)
    if (all(d$W + d$L + d$T == 0, na.rm = TRUE)) {
      return(p(em("Seeds appear once games have been played. ESPN ranks teams with the ",
                  "full NFL tiebreaker procedure, so this stays accurate through Week 18."),
               style = "color: #666;"))
    }

    seed_list <- function(conf_abbr, color) {
      s <- d %>% filter(Conference == conf_abbr, !is.na(Seed), Seed <= 7) %>% arrange(Seed)
      tagList(
        h4(paste(conf_abbr, "Playoff Seeds"), style = paste0("color:", color, ";")),
        lapply(seq_len(nrow(s)), function(i) {
          r <- s[i, ]
          badge <- if (r$Seed == 1) "#6f42c1" else if (r$Seed <= 4) "#28a745" else "#ffc107"
          div(style = "padding: 5px 0; border-bottom: 1px solid #eee;",
              span(style = paste0("background:", badge, "; color: #fff; padding: 2px 8px; ",
                                  "border-radius: 3px; font-weight: bold; margin-right: 10px;"),
                   paste0("#", r$Seed)),
              strong(r$Team),
              span(style = "color: #666; margin-left: 8px;",
                   paste0(r$W, "-", r$L, if (!is.na(r$T) && r$T > 0) paste0("-", r$T) else "")),
              if (r$Seed == 1) span(style = "color: #6f42c1; margin-left: 8px; font-weight: bold;", "first-round bye"),
              if (r$Seed > 4)  span(style = "color: #856404; margin-left: 8px;", "wild card"),
              if (!is.na(r$Clinch) && nzchar(r$Clinch))
                span(style = "color: #666; margin-left: 8px;", paste0("(", r$Clinch, " — ", r$ClinchDesc, ")")))
        })
      )
    }

    fluidRow(
      column(6, seed_list("AFC", "#c8102e")),
      column(6, seed_list("NFC", "#013369")),
      column(12, p(em("z clinched division · y clinched wild card · x clinched playoff berth · ",
                      "* clinched bye / home field · e eliminated"),
                   style = "color: #888; font-size: 0.85em; margin-top: 10px;"))
    )
  })

  # Every output below lives in a tab that is hidden at page load, and each is
  # wrapped in withSpinner(). withSpinner() hides the real output element while
  # the spinner shows, which Shiny's default suspendWhenHidden reads as "not
  # visible" — so the output is suspended, never computes, and the spinner spins
  # forever. Opting out of suspension is what breaks that deadlock. Expensive
  # fetches still defer via the req() on their tab title above.
  outputOptions(output, "hourly_forecast_enhanced", suspendWhenHidden = FALSE)
  outputOptions(output, "gameday_analysis",         suspendWhenHidden = FALSE)
  outputOptions(output, "week_overview",            suspendWhenHidden = FALSE)
  outputOptions(output, "week_overview_label",      suspendWhenHidden = FALSE)
  outputOptions(output, "scores_caption",           suspendWhenHidden = FALSE)
  outputOptions(output, "scoreboard_table",         suspendWhenHidden = FALSE)
  outputOptions(output, "afc_standings",            suspendWhenHidden = FALSE)
  outputOptions(output, "nfc_standings",            suspendWhenHidden = FALSE)
  outputOptions(output, "playoff_picture",          suspendWhenHidden = FALSE)
}

# 7. RUN THE APPLICATION ----
shinyApp(ui = ui, server = server)