
# Basic shinydashboard from
# https://rstudio.github.io/shinydashboard/get_started.html

# app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
