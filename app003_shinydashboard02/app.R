
# Basic shinydashboard from
# https://rstudio.github.io/shinydashboard/get_started.html

# Tried to add button to change tab, that didn't work
# (loosely following https://mastering-shiny.org/action-dynamic.html#dynamic-wizard) 

# app.R ##
library(shiny)
library(shinydashboard)
library(DT)              # DT interactive tables
library(ggplot2)    

table1 <- data.frame(
  Var1 = letters[1:5],
  Var2 = 10:14
)

table2 <- data.frame(
  Var1 = rep(letters[1:5], each = 3),
  Var3 = seq(100, length = 15)
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
        h3("Widgets tab content"),
        dataTableOutput("table2_dt"),
        plotOutput("table2_plot")
      )
    )
  )
)


server <- function(input, output) {

  output$table1_dt <- renderDataTable(
    table1,
    server = TRUE
  )

  output$table2_plot <- renderPlot(
    ggplot(table2, aes(Var1, Var3)) +
      geom_point()
  )
  
  output$table2_dt <- renderDataTable(
    table2,
    server = TRUE
  )
  
  
}

shinyApp(ui, server)
