library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggridges)
library(ggpubr)
library(ggstatsplot)
library(plotly)
library(ggthemes)
library(hrbrthemes)
library(highcharter)
library(vistime)
library(shinymanager)
library(DT)
library(reactable)
library(reactablefmtr)
library(vroom)
library(shinyjs)

source("R/loadData.R")  

# JavaScript code to handle 'Enter' key behavior
js <- "
pressbtn = function(){
    // Click the log in button
    document.getElementById('auth-go_auth').click();
};
window.onload = function() {
    // Password input field
    const field = document.getElementById('auth-user_pwd');

    // Add a function that preempts the 'Enter' key press
    field.addEventListener('keydown', function(e) {
        if (e.keyCode == 13) {
            // Prevent sending the key event
            e.preventDefault();
            // Delay activating the login button for 400 ms. Adjust time as needed
            setTimeout(pressbtn, 400);
        }
    });
}
"

# Define credentials
credentials <- data.frame(
  user = c("wriiscUser"),
  password = c("cmi2024"),
  stringsAsFactors = FALSE
)

# UI
ui <- secure_app(
  dashboardPage(
    dashboardHeader(title = "WRIISC CMI-Dash"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Read Me", tabName = "readMe", icon = icon("info-circle")),
        menuItem("Exposures, CMI & Diagnoses",
                 tabName = "exposuresCmiDx",
                 icon = icon("bolt")),
        menuItem("Diagnoses & Exposures",
                 tabName = "ExpoAndDx",
                 icon = icon("heartbeat"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        readMeUI("firstMenuItem"),
        exposuresCmiDxUI("thirdMenuItem"),
        expoAndDxUI("fourthMenuItem")
      )
    )
  ),
  head_auth = tags$script(js)  # Inject the JavaScript into the authentication page
)

# Server
server <- function(input, output, session) {
  # Call the server part of shinymanager
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Your server logic
  readMeServer("firstMenuItem")
  exposuresCmiDxServer("thirdMenuItem")
  expoAndDxServer("fourthMenuItem")
}

# Run the app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
