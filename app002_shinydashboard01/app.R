
# Basic shinydashboard from
# https://rstudio.github.io/shinydashboard/get_started.html

# Tried to add button to change tab, that didn't work
# (loosely following https://mastering-shiny.org/action-dynamic.html#dynamic-wizard) 

# app.R ##
library(shiny)
library(shinydashboard)

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
          box(plotOutput("plot1", height = 250)),
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        ),
        fluidRow(
          actionButton("page_12", "next")
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
  set.seed(122)

  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
