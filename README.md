NFL Weather RShiny App (2026 Season)
=====================================

**Live Application:** [Experience the NFL Weather Dashboard on ShinyApps](https://rcuevas.shinyapps.io/NFL_Weather/)

An interactive R Shiny dashboard providing detailed weather forecasts and game-day impact analysis for the 2026 NFL regular season — all 272 games across Weeks 1–18.

This application is built for football fans, fantasy players, and analysts who want to understand how weather conditions will affect game day. It moves beyond a simple forecast by providing custom, data-driven scores for specific gameplay elements and a clear, at-a-glance impact assessment for every game in the 2026 season.

<img width="5114" height="2552" alt="image" src="https://github.com/user-attachments/assets/cc527367-6f45-4ed7-b12f-11ff8f5f5347" />

Key Features
------------

*   **Interactive Filtering:** View the entire 2026 NFL schedule by Week, Stadium, Team, or a specific Date.
*   **Dynamic UI:** The interface updates intelligently based on your selections to show you the most relevant games.
*   **Kickoff-Based Conditions:** The headline temperature, wind, and precipitation figures describe the forecast period that actually covers kickoff — not whatever the weather happens to be doing at the venue right now. When a game is still beyond the forecast horizon, the dashboard says so plainly and falls back to current venue conditions rather than passing them off as the game forecast.
*   **Live Weather Data:** Utilizes the National Weather Service (NWS) API for up-to-date daily and hourly forecast information, plus real-time ASOS observations from the Iowa Environmental Mesonet. A **Refresh Weather Data** button clears the cache and re-pulls the latest conditions on demand.
*   **Game Impact Assessment:** A color-coded system (Green, Yellow, Red) provides an immediate sense of the potential for weather to disrupt a game.
*   **Custom Gameplay Scores:**
    -   **Kicking Score (1-10):** A unique score that heavily weights wind, precipitation, and cold to grade the difficulty of the kicking game.
    -   **Passing Score (1-10):** A second score that analyzes wind, precipitation, and extreme temperatures to grade the conditions for the passing game.
    -   **Rush Advantage (0-10):** Grades how strongly the weather favors the running game over the pass (cold, precipitation, and snow push this higher).
*   **Wind vs. Field Orientation:** Uses each stadium's long-axis bearing to classify kickoff wind as Along-Field, Diagonal, or Crosswind.
*   **In-Depth Analysis Tabs:**
    -   **7-Day Outlook:** A summary of the week's forecast with impact ratings for each period.
    -   **Hourly Detail:** A detailed, 48-hour forecast showing how conditions will evolve around kickoff.
    -   **Game Analysis:** A dedicated view of the conditions before, during, and after a selected game, including the custom Kicking, Passing, and Rush Advantage scores.
    -   **Week Overview:** A master table of every game for a given week, with the ability to filter out games played in domes. It follows whichever game you have selected, so it always describes the same week as the rest of the dashboard.
*   **Domes and International Venues Handled Explicitly:** Indoor stadiums are flagged and skip weather lookups entirely. The 2026 slate includes eight games outside the United States (Melbourne, Rio de Janeiro, London ×2, Paris, Madrid, Munich, and Monterrey); these sit outside National Weather Service coverage and are labelled as such instead of showing a misleading forecast.

How to Run Locally
------------------

To run this application on your own machine, follow these steps.

**Prerequisites:**
-   Make sure you have R (https://cran.r-project.org/) and RStudio (https://posit.co/download/rstudio-desktop/) installed.

1. **Clone the Repository**
Open your terminal or command prompt and clone this repository to your local machine:

    ```bash
    git clone https://github.com/Cuevman81/NFL_Weather_Shiny_App.git
    cd NFL_Weather_Shiny_App
    ```

2. **Install Required Packages**
Open the `NFL_Weather_app.R` file in RStudio. In the RStudio console, run the following command to install all the necessary packages:

    ```r
    install.packages(c("shiny", "shinydashboard", "dplyr", "httr", "jsonlite", "lubridate", "DT", "here", "shinycssloaders"))
    ```

3. **Run the Application**
With the `NFL_Weather_app.R` file open in RStudio, click the **Run App** button located at the top-right of the editor pane. The application will launch in a new window or in your default web browser.

Data Sources
------------

This application relies on the following sources for its data:

*   **Weather Forecasts:** National Weather Service (NWS) API (https://www.weather.gov/documentation/services-web-api) for all forecast data.
*   **Real-Time Conditions:** ASOS/METAR observations from the Iowa Environmental Mesonet (https://mesonet.agron.iastate.edu/), requested directly so the call can be given a hard timeout. Verified working from the live shinyapps.io deployment; if a station is unreachable the dashboard falls back to the NWS forecast rather than failing.
*   **Schedule Data:** The `nfl_schedule_2026_detailed.csv` file included in this repository.

Both sources are cached in-process — forecasts for 10 minutes, observations for 5 — so repeated views of the same venue don't re-hit the APIs. Every outbound request has a 12-second timeout.

Repository Contents
-------------------

| File | Role |
|---|---|
| `NFL_Weather_app.R` | The weather dashboard — this is the deployed app |
| `NFL_Schedule_Explorer.R` | Companion season-overview app, run locally |
| `NFL_Weather_CSV_Creater_2026.R` | Builds the detailed schedule; holds the stadium dictionary (coordinates, timezone, ICAO station, surface, dome status, field orientation) |
| `nfl_schedule_2026.csv` | Base schedule — input to the builder |
| `nfl_schedule_2026_detailed.csv` | Builder output — read by both apps |

Only `NFL_Weather_app.R` and `nfl_schedule_2026_detailed.csv` are deployed. When games change, edit `nfl_schedule_2026.csv` and re-run the builder.

Contact
-------

This application was developed by Rodney Cuevas, Meteorologist & Developer.

For any bugs, comments, or suggestions, please feel free to reach out via email at RodneyJCuevas@gmail.com.
