
# Basic shinydashboard from
# https://rstudio.github.io/shinydashboard/get_started.html

# Tried to add button to change tab, that didn't work
# (loosely following https://mastering-shiny.org/action-dynamic.html#dynamic-wizard) 

# app.R ##
library(shiny)
library(shinydashboard)
library(DT)              # DT interactive tables

table1 <- data.frame(
  Var1 = letters[1:5],
  var2 = 10:14
)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "page_1", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "page_2", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "page_1",
        fluidRow(
          box(
            dataTableOutput("table1_dt"))
        ),
        fluidRow(
          h4("test content")
        )
      ),
      
      # Second tab content
      tabItem(
        tabName = "page_2",
        h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {

  output$table1_dt <- renderDataTable(
    table1,
    server = TRUE
  )
  
  
}

shinyApp(ui, server)
