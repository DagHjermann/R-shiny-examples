#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)              # DT interactive tables

# Example data - see https://github.com/tidyverse/nycflights13 
# - also see https://r4ds.had.co.nz/relational-data.html 

library(nycflights13)    # example data 

origin_airports <- unique(flights$origin)

origin_table_data <- airports %>%
  filter(faa %in% origin_airports)

# UI layout is the "Wizard interface" from Mastering Shiny  
# https://mastering-shiny.org/action-dynamic.html#dynamic-wizard 
ui <- fluidPage(
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1", 
             h3("Select airports"),
             DT::dataTableOutput('origin_table'),
             actionButton("page_12", "next")
    ),
    tabPanel("page_2",
             h4("Selected airports"),
             DT::dataTableOutput('origin_selection'),
             h3("Select months"),
             DT::dataTableOutput('flights_by_month_table'),
             actionButton("page_21", "prev"),
             actionButton("page_23", "next")
    ),
    tabPanel("page_3", 
             textOutput('months_sel'),
             actionButton("page_32", "prev")
    )
  )
)

server <- function(input, output, session) {
  
  # 
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
  
  # . output$origin_table ----
  output$origin_table <- renderDataTable(
    origin_table_data, 
    server = TRUE
  )
  
  origin_table_data_sel <- reactive(
    origin_table_data[input$origin_table_rows_selected,]
  )
  
  # . output$origin_selection ----
  output$origin_selection <- renderDataTable(
    origin_table_data_sel(),
    server = TRUE, 
    selection = 'none'
  )
  
  flights_by_month <- reactive({
    flights %>%
      filter(origin %in% origin_table_data_sel()$faa) %>%
      count(month, name = "Number_of_flights")
  })
  
  # . output$flights_by_month_table ----
  output$flights_by_month_table <- renderDataTable(
    flights_by_month(), 
    server = TRUE
  )
  
  flights_month_by_dest <- reactive({
    flights %>%
      filter(origin %in% origin_table_data_sel()$faa,
             month %in% input$flights_by_month_table_rows_selected) %>%
      count(dest, name = "Number_of_flights")
  })
  
  # . output$flights_month_by_dest_table ----
  output$flights_month_by_dest_table <- renderDataTable(
    flights_month_by_dest(), 
    server = TRUE
  )
  
  months_sel <- reactive({
    flights_month_by_dest()[flights_by_month_table_rows_selected,] %>%
      pull(month)
  })
  
  output$months_sel <- renderText(
    paste("Months selected:", paste(months_sel(), collapse = ", "))
    )
  

}

# Run the application 
shinyApp(ui = ui, server = server)
