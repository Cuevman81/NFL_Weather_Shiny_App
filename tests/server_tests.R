#!/usr/bin/env Rscript
# Server tests for NFL_Weather_app.R (and the Explorer's playoff seeds).
#
# Runs the real server() through shiny::testServer with a faked clock and every
# HTTP call stubbed (NWS, IEM, ESPN), so it needs no network and no keys. Run it
# the way shinyapps.io runs the app, with the process in UTC:
#
#   TZ=UTC Rscript --vanilla tests/server_tests.R            # this repo
#   TZ=UTC Rscript --vanilla tests/server_tests.R <app_dir>  # another copy
#
# Every check prints PASS or FAIL; the script exits 1 if any check fails.

suppressPackageStartupMessages({
  library(shiny)
  library(jsonlite)
  library(lubridate)
})

args <- commandArgs(trailingOnly = TRUE)
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
app_dir <- normalizePath(if (length(args)) args[1] else file.path(dirname(this_file), ".."))

# ---- Fakes ------------------------------------------------------------------
FAKE <- new.env()
set_now <- function(utc) FAKE$now <- .POSIXct(as.numeric(as.POSIXct(utc, tz = "UTC")))
advance <- function(secs) FAKE$now <- FAKE$now + secs
calm <- function(i, start, hourly) list(temp = 61, wind = "5 mph", dir = "S", pop = 0, short = "Sunny", rh = 50)
reset_fakes <- function() {
  FAKE$weather <- calm
  FAKE$calls <- character(0)
  FAKE$fail <- FALSE
  FAKE$events <- list()
  FAKE$iem_csv <- "station,valid,tmpf,sknt,drct\n"
  FAKE$iem_query <- NULL
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")

# NWS-shaped forecast: 14 day/night periods on 6 AM / 6 PM Eastern boundaries
# (the first starting now, as NWS's does), or 156 hourly periods.
nws_json <- function(hourly) {
  now <- FAKE$now
  if (hourly) {
    starts <- floor_date(with_tz(now, "UTC"), "hour") + 3600 * (0:155)
    ends <- starts + 3600
  } else {
    days <- as.Date(with_tz(now, "America/New_York")) + 0:8
    b <- sort(c(as.POSIXct(paste(days, "06:00"), tz = "America/New_York"),
                as.POSIXct(paste(days, "18:00"), tz = "America/New_York")))
    ends <- b[b > now][1:14]
    starts <- c(now, ends[-14])
  }
  periods <- lapply(seq_along(starts), function(i) {
    w <- FAKE$weather(i, starts[i], hourly)
    p <- list(number = i,
              name = if (hourly) "" else format(with_tz(starts[i], "America/New_York"), "%A"),
              startTime = iso(starts[i]), endTime = iso(ends[i]), isDaytime = TRUE,
              temperature = w$temp, temperatureUnit = "F",
              probabilityOfPrecipitation = list(unitCode = "wmoUnit:percent", value = w$pop),
              windSpeed = w$wind, windDirection = w$dir, shortForecast = w$short)
    if (hourly) p$relativeHumidity <- list(unitCode = "wmoUnit:percent", value = w$rh)
    p
  })
  toJSON(list(properties = list(periods = periods)), auto_unbox = TRUE, na = "null")
}

espn_scoreboard_json <- function() {
  toJSON(list(events = lapply(FAKE$events, function(e) list(competitions = list(list(
    competitors = list(
      list(homeAway = "home", team = list(abbreviation = e$home), score = e$hs, records = list(list(summary = "1-2"))),
      list(homeAway = "away", team = list(abbreviation = e$away), score = e$as, records = list(list(summary = "2-1")))),
    status = list(type = list(name = e$status, detail = e$detail, completed = FALSE))))))),
    auto_unbox = TRUE)
}

# ESPN standings: 8 divisions of 4. Only the first team in each division has
# played and been seeded; the rest are 0-0 with ESPN's playoffSeed of 0.
espn_standings_json <- function() {
  conf <- function(abbr, first_seed) {
    list(abbreviation = abbr, children = lapply(0:3, function(d) {
      list(name = paste(abbr, c("East", "North", "South", "West")[d + 1]),
           standings = list(entries = lapply(1:4, function(t) {
             seeded <- t == 1
             st <- function(n, v) list(name = n, value = v, displayValue = as.character(v))
             list(team = list(displayName = paste(abbr, d, t, if (seeded) "Seeded" else "Unplayed"),
                              abbreviation = paste0(abbr, d, t)),
                  stats = list(st("wins", if (seeded) 1 else 0), st("losses", 0), st("ties", 0),
                               st("winPercent", if (seeded) 1 else 0), st("pointsFor", if (seeded) 24 else 0),
                               st("pointsAgainst", if (seeded) 10 else 0),
                               st("playoffSeed", if (seeded) first_seed + d else 0)))
           })))
    }))
  }
  toJSON(list(children = list(conf("AFC", 1), conf("NFC", 1))), auto_unbox = TRUE)
}

make_resp <- function(url, body, status = 200L, type = "application/json") {
  structure(list(url = url, status_code = as.integer(status),
                 headers = structure(list(`content-type` = type), class = c("insensitive", "list")),
                 all_headers = list(), content = charToRaw(enc2utf8(as.character(body))),
                 date = Sys.time(), times = c(total = 0)),
            class = "response")
}

stub_GET <- function(url, ..., query = NULL) {
  FAKE$calls <- c(FAKE$calls, url)
  if (grepl("mesonet", url)) {
    FAKE$iem_query <- query
    return(make_resp(url, FAKE$iem_csv, type = "text/plain"))
  }
  if (grepl("espn.com", url)) {
    if (grepl("scoreboard", url)) return(make_resp(url, espn_scoreboard_json()))
    if (isTRUE(FAKE$standings)) return(make_resp(url, espn_standings_json()))
    return(make_resp(url, "{}", 500L))
  }
  if (FAKE$fail) return(make_resp(url, "{}", 500L))
  if (grepl("/points/", url)) {
    ll <- sub(".*/points/", "", url)
    return(make_resp(url, toJSON(list(properties = list(
      forecast = paste0("https://api.weather.gov/gridpoints/FAKE/", ll, "/forecast"),
      forecastHourly = paste0("https://api.weather.gov/gridpoints/FAKE/", ll, "/forecast/hourly"))),
      auto_unbox = TRUE)))
  }
  if (grepl("/forecast/hourly$", url)) return(make_resp(url, nws_json(hourly = TRUE)))
  if (grepl("/forecast$", url)) return(make_resp(url, nws_json(hourly = FALSE)))
  stop("unexpected URL in test: ", url)
}

# Source an app file into a fresh environment whose Sys.time/Sys.Date/GET are
# the fakes. The app's own functions resolve those names there first.
load_app <- function(file = "NFL_Weather_app.R", tz = "UTC") {
  Sys.setenv(TZ = tz)  # the server's zone before the app starts (shinyapps.io: UTC)
  env <- new.env(parent = globalenv())
  env$Sys.time <- function() FAKE$now
  env$Sys.Date <- function() as.Date(as.POSIXlt(FAKE$now))
  env$GET <- stub_GET
  old <- setwd(app_dir); on.exit(setwd(old))
  suppressMessages(source(file.path(app_dir, file), local = env))
  env
}

# ---- Assertions and readers -------------------------------------------------
results <- c()
check <- function(name, ok, detail = "") {
  ok <- isTRUE(ok)
  detail <- paste(detail, collapse = " ")
  results[name] <<- ok
  cat(if (ok) "PASS " else "FAIL ", name, if (nzchar(detail)) paste0("  [", detail, "]"), "\n", sep = "")
}

selected_label <- function(ui) {
  m <- regmatches(ui$html, regexec('<option value="[^"]*" selected>([^<]*)</option>', ui$html))[[1]]
  if (length(m) < 2) NA_character_ else m[2]
}
dt_cols <- function(json) {  # DT sends its data column by column
  lapply(fromJSON(as.character(json), simplifyVector = FALSE)$x$data, function(col)
    vapply(col, function(v) if (is.null(v)) NA_character_ else as.character(v), ""))
}
badge_status <- function(html) sub('.*(GREEN|YELLOW|RED|TBD|DOME)</span>.*', "\\1", html)
row_of <- function(cols, pattern) which(grepl(pattern, cols[[1]], fixed = TRUE))[1]
map_popups <- function(json) {
  calls <- fromJSON(as.character(json), simplifyVector = FALSE)$x$calls
  mk <- Filter(function(cl) cl$method == "addCircleMarkers", calls)[[1]]$args
  vecs <- Filter(function(a) is.list(a) && length(a) > 0 && all(vapply(a, is.character, TRUE)), mk)
  popup <- unlist(Filter(function(v) any(grepl("font-size:13px", unlist(v))), vecs)[[1]])
  label <- unlist(Filter(function(v) any(grepl(" @ ", unlist(v))) && !any(grepl("<div", unlist(v))), vecs)[[1]])
  setNames(popup, label)
}
game_id <- function(env, matchup, week) env$schedule_data$game_id[env$schedule_data$Matchup == matchup &
                                                                   env$schedule_data$Week == week]

cat("App:", app_dir, "\n")

# ---- #1: a UTC server must not drop tonight's primetime game ------------------
reset_fakes()
set_now("2026-09-28 00:05:00")          # 8:05 PM EDT Sun Sep 27, 15 min before LA @ DEN
app <- load_app()
check("#1 date picker min is the Eastern date (2026-09-27)",
      grepl('data-min-date="2026-09-27"', as.character(app$ui), fixed = TRUE))
testServer(app$server, {
  session$setInputs(selection_method = "team", selected_team = "DEN", hide_domes = FALSE,
                    main_tabs = "7-Day Outlook", scores_week = "3", map_color_by = "weather")
  lbl <- selected_label(output$team_games_selector)
  check("#1 Team picker opens on tonight's LA @ DEN", identical(lbl, "Sep 27 - LA @ DEN (06:20 PM MDT)"), lbl)
  session$setInputs(selection_method = "stadium", selected_stadium = "Empower Field at Mile High")
  lbl <- selected_label(output$stadium_games_selector)
  check("#1 Stadium picker opens on tonight's LA @ DEN", identical(lbl, "Sep 27 - LA @ DEN (06:20 PM MDT)"), lbl)
  session$setInputs(selection_method = "week", selected_week = "3", main_tabs = "Week Overview")
  cols <- dt_cols(output$week_overview); i <- row_of(cols, "LA @ DEN")
  check("#1 Week Overview rates LA @ DEN (not TBD)",
        badge_status(cols[[4]][i]) == "GREEN" && cols[[6]][i] == "61",
        paste(badge_status(cols[[4]][i]), cols[[6]][i]))
  session$setInputs(main_tabs = "Game Map")
  pop <- map_popups(output$game_map)
  check("#1 Game Map shows LA @ DEN's forecast, not 'Already played'",
        grepl("61&deg;F", pop[["LA @ DEN"]]) && !grepl("Already played", pop[["LA @ DEN"]]))
  check("#1 'Data updated' stamp is Eastern", grepl("ED?T", output$weather_last_updated), output$weather_last_updated)
})

# ---- #2: no borrowing today's weather for a game a week out -------------------
reset_fakes()
set_now("2026-09-27 15:00:00")          # 11 AM EDT Sun Sep 27, looking at Week 4
FAKE$weather <- function(i, start, hourly) {
  if (!hourly && i == 1) list(temp = 84, wind = "20 to 30 mph", dir = "SW", pop = 90,
                              short = "Showers And Thunderstorms", rh = NA)
  else calm(i, start, hourly)
}
app <- load_app()
testServer(app$server, {
  session$setInputs(selection_method = "week", selected_week = "4", hide_domes = FALSE,
                    main_tabs = "Week Overview", scores_week = "4", map_color_by = "weather")
  cols <- dt_cols(output$week_overview)
  st <- badge_status(cols[[4]])
  sunday <- grepl("^Oct 0[45]", cols[[1]]) & st != "DOME"
  check("#2 no Week 4 Sunday/Monday game is rated on today's storm (all TBD)",
        !any(st[sunday] == "RED") && all(st[sunday] %in% c("TBD")),
        paste0(sum(st[sunday] == "RED"), " RED of ", sum(sunday)))
  i <- row_of(cols, "PIT @ CLE")
  check("#2 Thursday's PIT @ CLE, inside the forecast, is still rated", badge_status(cols[[4]][i]) == "GREEN",
        badge_status(cols[[4]][i]))
  session$setInputs(main_tabs = "Game Map")
  pop <- map_popups(output$game_map)
  check("#2 map: a game beyond the forecast says so", grepl("beyond the NWS forecast range", pop[["NYJ @ CHI"]]),
        gsub("<[^>]+>", " ", sub(".*<hr[^>]*>", "", pop[["NYJ @ CHI"]])))
  check("#2 map: London game says outside NWS coverage", grepl("Outside NWS coverage", pop[["IND @ WAS"]]),
        gsub("<[^>]+>", " ", sub(".*<hr[^>]*>", "", pop[["IND @ WAS"]])))
})
# A game under way still shows the current period.
reset_fakes()
set_now("2026-09-27 18:00:00")          # 2 PM EDT, the 1 PM games are on
app <- load_app()
testServer(app$server, {
  session$setInputs(selection_method = "week", selected_week = "3", hide_domes = FALSE,
                    main_tabs = "Week Overview", scores_week = "3", map_color_by = "weather")
  cols <- dt_cols(output$week_overview); i <- row_of(cols, "LAC @ BUF")
  check("#2 a 1 PM game checked at 2 PM (under way) keeps current conditions",
        badge_status(cols[[4]][i]) == "GREEN" && cols[[6]][i] == "61",
        paste(badge_status(cols[[4]][i]), cols[[6]][i]))
})

# ---- #4: feels-like grades the tables, not just the headline ------------------
reset_fakes()
set_now("2026-12-05 15:00:00")          # Sat 9 AM CST; JAX @ CHI Sun Dec 6 at noon CST
FAKE$weather <- function(i, start, hourly) list(temp = 34, wind = "15 to 20 mph", dir = "NW", pop = 10,
                                                short = "Mostly Cloudy", rh = 70)
app <- load_app()
gid <- game_id(app, "JAX @ CHI", 13)
testServer(app$server, {
  session$setInputs(selection_method = "team", selected_team = "CHI", hide_domes = FALSE,
                    main_tabs = "Hourly Detail", scores_week = "13", map_color_by = "weather")
  session$setInputs(selected_team_game = as.character(gid))
  alert <- output$current_weather_alert$html
  check("#4 headline: Moderate Impact, feels like 23°F",
        grepl("Moderate Impact", alert) && grepl("23°F", alert))
  cols <- dt_cols(output$hourly_forecast_enhanced); i <- which(cols[[1]] == "Sun 12:00 PM")
  check("#4 hourly row at kickoff matches the headline (YELLOW)", badge_status(cols[[2]][i]) == "YELLOW",
        badge_status(cols[[2]][i]))
  cols <- dt_cols(output$daily_forecast_enhanced); i <- which(cols[[1]] == "Sunday")[1]
  check("#4 7-Day row for Sunday is YELLOW", badge_status(cols[[2]][i]) == "YELLOW", badge_status(cols[[2]][i]))
  session$setInputs(main_tabs = "Week Overview")
  cols <- dt_cols(output$week_overview); i <- row_of(cols, "JAX @ CHI")
  check("#4 Week Overview row JAX @ CHI is YELLOW", badge_status(cols[[4]][i]) == "YELLOW", badge_status(cols[[4]][i]))
  session$setInputs(main_tabs = "Game Analysis")
  ga <- output$gameday_analysis$html
  check("#4 During Game lists the wind-chill factor", grepl("Critical Factors", ga) && grepl("Freezing conditions", ga))
  check("#4 Impact Factors box uses feels-like", grepl("Freezing conditions", output$impact_factors$html))
})

# ---- #5: Refresh cooldown, /points kept, failures cached ---------------------
reset_fakes()
set_now("2026-09-27 15:00:00")
app <- load_app()
n_points <- function() sum(grepl("/points/", FAKE$calls))
invisible(app$get_nws_forecast(41.8623, -87.6167))
advance(11 * 60)                        # forecast cache expired
invisible(app$get_nws_forecast(41.8623, -87.6167))
check("#5 /points fetched once per stadium, not on every forecast refresh", n_points() == 1, paste(n_points(), "calls"))
FAKE$fail <- TRUE; FAKE$calls <- character(0)
invisible(app$get_nws_forecast(39.7439, -105.0201)); invisible(app$get_nws_forecast(39.7439, -105.0201))
check("#5 a failed forecast is not retried on the next render", length(FAKE$calls) == 1, paste(length(FAKE$calls), "calls"))
advance(3 * 60); invisible(app$get_nws_forecast(39.7439, -105.0201))
check("#5 ... but is retried after 2 minutes", length(FAKE$calls) == 2, paste(length(FAKE$calls), "calls"))
FAKE$fail <- FALSE
# (testServer sees the server's own locals; the app's top-level objects are app$...)
testServer(app$server, {
  session$setInputs(selection_method = "stadium", selected_stadium = "Soldier Field", main_tabs = "7-Day Outlook",
                    hide_domes = FALSE, scores_week = "3", map_color_by = "weather")
  invisible(app$get_nws_forecast(41.8623, -87.6167))
  session$setInputs(refresh_weather = 1)
  # (Re-renders may re-cache the scoreboard straight away; the forecast key is the test.)
  key <- "41.862,-87.617_d"
  check("#5 first Refresh clears the shared cache", !key %in% ls(app$.nws_cache))
  invisible(app$get_nws_forecast(41.8623, -87.6167))
  advance(10); r1 <- weather_refresh()
  session$setInputs(refresh_weather = 2)
  check("#5 a second Refresh within a minute leaves the shared cache alone", key %in% ls(app$.nws_cache))
  check("#5 ... but still re-renders the clicker's view", !identical(weather_refresh(), r1))
  advance(61)
  session$setInputs(refresh_weather = 3)
  check("#5 Refresh after the cooldown clears it again", !key %in% ls(app$.nws_cache))
})

# ---- #6: the week doesn't roll over during its last game --------------------
reset_fakes()
set_now("2026-09-29 01:00:00")          # 9 PM EDT Mon Sep 28, during PHI @ CHI
app <- load_app()
testServer(app$server, {
  session$setInputs(selection_method = "week", selected_week = "3", main_tabs = "7-Day Outlook",
                    hide_domes = FALSE, scores_week = "3", map_color_by = "weather")
  check("#6 current week during MNF is still Week 3", current_nfl_week() == 3, current_nfl_week())
  check("#6 Week 3 picker still lists the MNF game in progress",
        grepl("PHI @ CHI", output$week_games_selector$html))
})

# ---- #7: upstream text is escaped in map popups ------------------------------
reset_fakes()
set_now("2026-09-28 00:40:00")          # LA @ DEN under way
FAKE$weather <- function(i, start, hourly) list(temp = 61, wind = "5 mph<b onmouseover=x>", dir = "S",
                                                pop = 0, short = "Sunny", rh = 50)
FAKE$events <- list(list(away = "LAR", home = "DEN", as = "7", hs = "3", status = "STATUS_IN_PROGRESS",
                         detail = "Q1 <img src=x onerror=alert(1)>"))
app <- load_app()
testServer(app$server, {
  session$setInputs(selection_method = "week", selected_week = "3", main_tabs = "Game Map",
                    hide_domes = FALSE, scores_week = "3", map_color_by = "weather")
  m <- output$game_map
  pop <- map_popups(m)[["LA @ DEN"]]
  check("#7 ESPN status text is escaped in the popup",
        grepl("&lt;img src=x", pop, fixed = TRUE) && !grepl("<img", pop, fixed = TRUE))
  check("#7 NWS wind text is escaped in the popup",
        grepl("&lt;b onmouseover", pop, fixed = TRUE) && !grepl("<b onmouseover", pop, fixed = TRUE))
  # ---- #9: basemap attribution --------------------------------------------
  check("#9 map credits Esri and OpenStreetMap as the service requires",
        grepl("Powered by", m, fixed = TRUE) && grepl("OpenStreetMap", m, fixed = TRUE) &&
          grepl("HERE, Garmin", m, fixed = TRUE))
})

# ---- #8: a stale ASOS report is not "Current" --------------------------------
reset_fakes()
set_now("2026-09-29 01:00:00")          # 9 PM EDT Mon; UTC is already Sep 29
app <- load_app()
FAKE$iem_csv <- "station,valid,tmpf,sknt,drct\nKDEN,2026-09-27 19:00,55.0,4.0,180\n"
obs <- app$get_current_observations("KDEN")
check("#8 a 30-hour-old report is not returned as current", is.null(obs))
q <- FAKE$iem_query
check("#8 the IEM window runs through tomorrow in UTC (day2 = Sep 30)",
      q$day2 == 30 && q$month2 == 9, paste0(q$month2, "/", q$day2))
rm(list = ls(app$.nws_cache), envir = app$.nws_cache)
FAKE$iem_csv <- "station,valid,tmpf,sknt,drct\nKDEN,2026-09-29 00:30,55.0,4.0,180\n"
obs <- app$get_current_observations("KDEN")
check("#8 a 30-minute-old report is still used", !is.null(obs) && obs$tmpf == 55)
gid <- game_id(app, "SEA @ DEN", 6)     # Oct 15: beyond the forecast, so the headline uses the obs
testServer(app$server, {
  session$setInputs(selection_method = "team", selected_team = "DEN", main_tabs = "7-Day Outlook",
                    hide_domes = FALSE, scores_week = "3", map_color_by = "weather")
  session$setInputs(selected_team_game = as.character(gid))
  alert <- output$current_weather_alert$html
  check("#8 observation time carries its date", grepl("Mon Sep 28, 06:30 PM MDT", alert, fixed = TRUE),
        sub(".*Observation Time:</strong>\\s*([^<]*).*", "\\1", alert))
})

# ---- Regression: every tab still renders -------------------------------------
reset_fakes()
set_now("2026-09-27 15:00:00")
app <- load_app()
gid <- game_id(app, "LA @ DEN", 3)
testServer(app$server, {
  session$setInputs(selection_method = "team", selected_team = "DEN", main_tabs = "7-Day Outlook",
                    hide_domes = FALSE, scores_week = "3", map_color_by = "network",
                    standings_view = "division")
  session$setInputs(selected_team_game = as.character(gid))
  check("regression: 7-Day table has 14 periods", length(dt_cols(output$daily_forecast_enhanced)[[1]]) == 14)
  session$setInputs(main_tabs = "Hourly Detail")
  check("regression: Hourly table has 48 rows", length(dt_cols(output$hourly_forecast_enhanced)[[1]]) == 48)
  session$setInputs(main_tabs = "Game Analysis")
  check("regression: Game Analysis renders kickoff block", grepl("Kickoff Conditions", output$gameday_analysis$html))
  session$setInputs(main_tabs = "Week Overview")
  check("regression: Week Overview lists all 16 Week 3 games", length(dt_cols(output$week_overview)[[1]]) == 16)
  session$setInputs(main_tabs = "Standings & Scores")
  check("regression: Standings degrade cleanly when ESPN fails",
        grepl("Standings unavailable", output$division_standings$html))
  session$setInputs(main_tabs = "Game Map")
  check("regression: Game Map has 16 markers (network colours)", length(map_popups(output$game_map)) == 16)
  session$setInputs(selection_method = "date", selected_date = as.Date("2026-09-27"))
  check("regression: Date picker lists Sep 27 games", grepl("LA @ DEN", output$date_games_selector$html))
})

# ---- #10: the Explorer never shows a "#0" seed -------------------------------
reset_fakes()
FAKE$standings <- TRUE
set_now("2026-09-10 12:00:00")
explorer <- load_app("NFL_Schedule_Explorer.R")
testServer(explorer$server, {
  session$flushReact()
  pp <- output$playoff_picture$html
  check("#10 Explorer playoff picture has no #0 seeds", grepl(">#1<", pp, fixed = TRUE) && !grepl(">#0<", pp, fixed = TRUE))
  d <- standings_data()
  first <- d$Team[match(unique(d$Division), d$Division)]
  check("#10 Explorer puts the seeded team first in each division", all(grepl("Seeded", first)),
        paste(sum(grepl("Seeded", first)), "of", length(first)))
})
FAKE$standings <- FALSE

cat(sprintf("\n%d of %d checks passed\n", sum(results), length(results)))
if (!all(results)) quit(status = 1)
