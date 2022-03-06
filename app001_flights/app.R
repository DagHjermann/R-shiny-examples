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

ui <- fluidPage(
  tabsetPanel(
    id = "wizard",
    type = "hidden",
    tabPanel("page_1", 
             DT::dataTableOutput('origin_table'),
             actionButton("page_12", "next")
    ),
    tabPanel("page_2", 
             "Only one page to go",
             actionButton("page_21", "prev"),
             actionButton("page_23", "next")
    ),
    tabPanel("page_3", 
             "You're done!",
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
