
# Inspired by
# https://ijlyttle.github.io/reactivity-three-ways-quarto/ 

library(shiny)
library(palmerpenguins)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Penguins"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("group_var", 
                        "Grouping columns",
                        c("species", "island", "sex"),
                        multiple = TRUE),
            selectInput("aggregation_var", 
                        "Aggregation columns",
                        c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                        multiple = TRUE),
            selectInput("aggregation_functions", 
                        "Aggregation functions",
                        c("mean", "min", "max"),
                        multiple = TRUE),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Original data"),
          dataTableOutput("table_original"),
          h2("Data summary"),
          dataTableOutput("table_summary")
          
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table_original <- renderDataTable(penguins)
    
    penguins_summ <- reactive({
      
      func <- lapply(input$aggregation_functions, get) 
      names(func) <- input$aggregation_functions
      
      penguins %>%
        group_by(across(all_of(input$group_var))) %>%
        summarise(across(all_of(input$aggregation_var), func, na.rm = TRUE))
      
    })
    
    output$table_summary <- renderDataTable(penguins_summ())

    }

# Run the application 
shinyApp(ui = ui, server = server)
