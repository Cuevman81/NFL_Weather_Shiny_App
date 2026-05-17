# NFL_Weather_CSV_Creater_2026.R
# Load necessary R packages
library(dplyr)
library(tidyr)
library(stringr)

# --- 1. Updated Stadium Information for 2026 ---
# Field_Orientation: long-axis bearing in degrees (0-179). NA for dome stadiums.
stadium_locations <- data.frame(
  stringsAsFactors = FALSE,
  Stadium = c("State Farm Stadium", "Mercedes-Benz Stadium", "M&T Bank Stadium", "Highmark Stadium", "Bank of America Stadium", "Soldier Field", "Paycor Stadium", "Huntington Bank Field", "AT&T Stadium", "Empower Field at Mile High", "Ford Field", "Lambeau Field", "NRG Stadium", "Lucas Oil Stadium", "EverBank Stadium", "GEHA Field at Arrowhead Stadium", "Allegiant Stadium", "SoFi Stadium", "Hard Rock Stadium", "U.S. Bank Stadium", "Gillette Stadium", "Caesars Superdome", "MetLife Stadium", "Lincoln Financial Field", "Acrisure Stadium", "Levi's Stadium", "Lumen Field", "Raymond James Stadium", "Nissan Stadium", "Northwest Stadium",
              "Maracana Stadium", "Melbourne Cricket Ground", "Stade de France", "Santiago Bernabéu", "FC Bayern Munich Arena", "Estadio Banorte", "Tottenham Hotspur Stadium", "Wembley Stadium"),
  City = c("Glendale", "Atlanta", "Baltimore", "Orchard Park", "Charlotte", "Chicago", "Cincinnati", "Cleveland", "Arlington", "Denver", "Detroit", "Green Bay", "Houston", "Indianapolis", "Jacksonville", "Kansas City", "Las Vegas", "Inglewood", "Miami Gardens", "Minneapolis", "Foxborough", "New Orleans", "East Rutherford", "Philadelphia", "Pittsburgh", "Santa Clara", "Seattle", "Tampa", "Nashville", "Landover",
           "Rio de Janeiro", "Melbourne", "Paris", "Madrid", "Munich", "Monterrey", "London", "London"),
  Latitude = c(33.5276, 33.7553, 39.2781, 42.7737, 35.2258, 41.8623, 39.0954, 41.5061, 32.7479, 39.7439, 42.34, 44.5013, 29.6847, 39.7601, 30.3239, 39.0489, 36.0908, 33.9535, 25.958, 44.9736, 42.0909, 29.951, 40.8135, 39.9008, 40.4468, 37.4033, 47.5952, 27.9759, 36.1665, 38.9077,
               -22.9121, -37.8199, 48.9244, 40.4531, 48.2188, 25.6866, 51.6044, 51.5560),
  Longitude = c(-112.2626, -84.4003, -76.6228, -78.7869, -80.8528, -87.6167, -84.516, -81.6995, -97.0929, -105.0201, -83.0456, -88.0622, -95.4107, -86.1639, -81.6373, -94.4839, -115.1837, -118.3392, -80.2389, -93.2582, -71.2643, -90.0812, -74.0745, -75.1675, -80.0158, -121.9697, -122.3316, -82.5033, -86.7713, -76.8645,
                -43.2302, 144.9834, 2.3601, -3.6883, 11.6247, -100.3161, -0.0661, -0.2797),
  TimeZone = c("America/Phoenix", "America/New_York", "America/New_York", "America/New_York", "America/New_York", "America/Chicago", "America/New_York", "America/New_York", "America/Chicago", "America/Denver", "America/New_York", "America/Chicago", "America/Chicago", "America/Indiana/Indianapolis", "America/New_York", "America/Chicago", "America/Los_Angeles", "America/Los_Angeles", "America/New_York", "America/Chicago", "America/New_York", "America/Chicago", "America/New_York", "America/New_York", "America/New_York", "America/Los_Angeles", "America/Los_Angeles", "America/New_York", "America/New_York", "America/New_York",
               "America/Sao_Paulo", "Australia/Melbourne", "Europe/Paris", "Europe/Madrid", "Europe/Berlin", "America/Monterrey", "Europe/London", "Europe/London"),
  Station_ICAO = c("KPHX", "KATL", "KBWI", "KBUF", "KCLT", "KMDW", "KCVG", "KCLE", "KGKY", "KDEN", "KDTW", "KGRB", "KHOU", "KIND", "KJAX", "KMCI", "KLAS", "KLAX", "KOPF", "KMSP", "KOWD", "KMSY", "KEWR", "KPHL", "KPIT", "KSJC", "KSEA", "KTPA", "KBNA", "KDCA",
                   "SBRJ", "YMML", "LFPG", "LEMD", "EDDM", "MMMX", "EGLC", "EGLL"),
  Dome = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
           FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
  Surface = c("Grass", "FieldTurf", "FieldTurf", "A-Turf Titan 50", "FieldTurf", "Kentucky Bluegrass", "FieldTurf", "Kentucky Bluegrass", "Hellas Matrix Turf", "Kentucky Bluegrass", "FieldTurf", "SISGrass (Hybrid)", "Hellas Matrix Turf", "Shaw Sports Turf", "Bermuda Grass", "Bermuda Grass", "Bermuda Grass", "Hellas Matrix Turf", "Bermuda Grass", "UBU Speed Series", "Grass", "FieldTurf", "FieldTurf", "Hybrid Grass", "Kentucky Bluegrass", "Bermuda Grass", "FieldTurf", "Bermuda Grass", "Bermuda Grass", "Bermuda Grass",
              "Grass", "Grass", "Grass", "Hybrid Grass", "Hybrid Grass", "Grass", "Hybrid Grass", "Hybrid Grass"),
  Field_Orientation = c(NA, NA, 22, 47, 8, 0, 11, 168, NA, 11, NA, 0, NA, NA, 0, 21, NA, NA, 19, NA, 22, NA, 22, 63, 28, 136, 170, 14, 12, 77,
                         0, 163, 0, NA, 15, 15, 0, 117)
)



# --- 2. Read the 2026 Schedule CSV ---
schedule_df <- read.csv("nfl_schedule_2026.csv", stringsAsFactors = FALSE)

# --- 3. Process the Schedule and Combine with Location Data ---
final_schedule <- schedule_df %>%
  mutate(Stadium = str_replace_all(Stadium, "Levi’s.*Stadium", "Levi's Stadium")) %>%
  separate(Matchup, into = c("Away_Team", "Home_Team"), sep = " @ ", remove = FALSE) %>%
  mutate(Away_Team = str_trim(Away_Team), Home_Team = str_trim(Home_Team)) %>%
  left_join(stadium_locations, by = "Stadium") %>%
  select(
    Week, Day, Date, Game_Time = Time, Matchup, Away_Team, Home_Team,
    Stadium, City, Latitude, Longitude, TimeZone, Station_ICAO, Dome, Surface,
    Field_Orientation, Network
  )

# --- 4. Save the Final Data to a CSV File ---
write.csv(final_schedule, "nfl_schedule_2026_detailed.csv", row.names = FALSE)

# Print a confirmation message to the console
print("The file 'nfl_schedule_2026_detailed.csv' has been created successfully.")
print(paste("Total games processed:", nrow(final_schedule)))
