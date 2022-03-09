
# Basic shinydashboard from
# https://rstudio.github.io/shinydashboard/get_started.html
# with DT tables instead 

# app.R ##
library(shiny)
library(shinydashboard)
library(DT)              # DT interactive tables
library(ggplot2)
library(dplyr)

#
# Data tables used
#   Note that Var1 is in both tables, but Table 2 has several rows for each value of Var1 
#   We show entire Table 1 and use it for selecting which parts of Table 2 we should show
#   The selected values of Var1 (from Table 1) is used to select parts of Table 2 
#

table1 <- data.frame(
  Var1 = letters[1:5],
  Var2 = 10:14
)

table2 <- data.frame(
  Var1 = rep(letters[1:5], each = 50),
  Var3 = rep(10:14, each = 50) + rnorm(5*50)
  )

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Selection", tabName = "page_1", icon = icon("dashboard")),
      menuItem("Results", tabName = "page_2", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # First tab content - table 1 (for selection)
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
      
      # Second tab content - the rows of table 2 corresponding selection in 
      #   table 1 (using Var1 which is the common variable)
      tabItem(
        tabName = "page_2",
        h4("Selected rows of table 1"),
        dataTableOutput("table1_sel_dt"),
        h4("Results from table 2"),
        dataTableOutput("table2_dt"),
        plotOutput("table2_plot")
      )
    )
  )
)


server <- function(input, output) {
  
  # Make DT data table for table 1 
  output$table1_dt <- renderDataTable(
    table1,
    server = TRUE
  )

  # Use selection in table 1 to select rows of table 2
  # The function returns Table 2 filtered by values of Var1 selected by the user
  table2_sel <- reactive({
    table1_sel <- table1[input$table1_dt_rows_selected,]
    table2 %>% 
      filter(Var1 %in% table1_sel$Var1)
  })
  
  # Table of selected table 1 values
  output$table1_sel_dt <- renderDataTable(
    table1[input$table1_dt_rows_selected,],
    server = TRUE,
    selection = 'none',
    options = list(lengthChange = FALSE)
  ) 
  
  # Plot of selected table 2 values
  output$table2_plot <- renderPlot(
    ggplot(table2_sel(), aes(Var1, Var3)) +
      geom_jitter(width = 0.2)
  )
  
  # Table of selected table 2 values
  output$table2_dt <- renderDataTable(
    table2_sel(),
    server = TRUE
  ) 
  
  
}

shinyApp(ui, server)
