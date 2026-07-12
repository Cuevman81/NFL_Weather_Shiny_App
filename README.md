NFL Weather RShiny App (2026 Season)
=====================================

**Live Application:** [Experience the NFL Weather Dashboard on ShinyApps](https://rcuevas.shinyapps.io/NFL_Weather/)

An interactive R Shiny dashboard providing detailed weather forecasts and game-day impact analysis for the 2026 NFL regular season and playoffs.

This application is built for football fans, fantasy players, and analysts who want to understand how weather conditions will affect game day. It moves beyond a simple forecast by providing custom, data-driven scores for specific gameplay elements and a clear, at-a-glance impact assessment for every game in the 2026 season.

<img width="5114" height="2552" alt="image" src="https://github.com/user-attachments/assets/cc527367-6f45-4ed7-b12f-11ff8f5f5347" />

Key Features
------------

*   **Interactive Filtering:** View the entire 2026 NFL schedule by Week, Stadium, Team, or a specific Date.
*   **Dynamic UI:** The interface updates intelligently based on your selections to show you the most relevant games.
*   **Live Weather Data:** Utilizes the National Weather Service (NWS) API for up-to-date daily and hourly forecast information, plus real-time ASOS observations via the `riem` package. A **Refresh Weather Data** button clears the cache and re-pulls the latest conditions on demand.
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
    -   **Week Overview:** A master table of every game for a given week, with the ability to filter out games played in domes.

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
    install.packages(c("shiny", "shinydashboard", "dplyr", "httr", "jsonlite", "lubridate", "DT", "here", "riem", "shinycssloaders"))
    ```

3. **Run the Application**
With the `NFL_Weather_app.R` file open in RStudio, click the **Run App** button located at the top-right of the editor pane. The application will launch in a new window or in your default web browser.

Data Sources
------------

This application relies on the following sources for its data:

*   **Weather Forecasts:** National Weather Service (NWS) API (https://www.weather.gov/documentation/services-web-api) for all forecast data.
*   **Real-Time Conditions:** The `riem` R package, which pulls METAR data from the Iowa Environmental Mesonet. *(Note: Real-time data may be unavailable when deployed on some servers due to network policies.)*
*   **Schedule Data:** The `nfl_schedule_2026_detailed.csv` file included in this repository.

Contact
-------

This application was developed by Rodney Cuevas, Meteorologist & Developer.

For any bugs, comments, or suggestions, please feel free to reach out via email at RodneyJCuevas@gmail.com.
