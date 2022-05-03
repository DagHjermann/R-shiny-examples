#
# Testing ggrepel options on map data 
# See 'https://ggrepel.slowkow.com/articles/examples.html'  
#

library(shiny)
library(ggplot2)
library(ggrepel)

dat <- read.csv("KartOgFigurgrunnlag2021.csv") %>%
  mutate(x = x/1000,
         y = y/1000)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Testing ggrepel options"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("force","force", value = 1),
            numericInput("force_pull","force_pull", value = 1),
            radioButtons("direction", "direction", choices = c("both", "x", "y"), selected = "both"),
            sliderInput("nudge_x", "nudge_x", 
                        min = -2, max = 2, step = 0.1, value = 0),
            sliderInput("nudge_y", "nudge_y", 
                        min = -2, max = 2, step = 0.1, value = 0),
            sliderInput("box.padding", "box.padding", 
                        min = 0, max = 2, step = 0.05, value = 0.25),
            sliderInput("point.padding", "point.padding", 
                        min = 0, max = 2, step = 0.05, value = 0),
            sliderInput("hjust", "hjust", 
                        min = -0.3, max = 1.3, step = 0.1, value = 0.5),
            sliderInput("vjust", "vjust", 
                        min = -0.3, max = 1.3, step = 0.1, value = 0.5),
            sliderInput("point.size", "point.size", 
                        min = 0, max = 3, step = 0.1, value = 1),
            sliderInput("min.segment.length", "min.segment.length", 
                        min = 0, max = 3, step = 0.1, value = 0.5),
            sliderInput("xlim", "xlim", 
                        min = 550, max = 850, step = 5, value = c(550,850)),
            numericInput("seed","seed", value = 100),
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("repel_plot", height = "650px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$repel_plot <- renderPlot({
      ggplot(dat, aes(x, y, label = Shortname)) +
        geom_text_repel(
          force = input$force,
          force_pull = input$force_pull,
          direction = input$direction,
          nudge_x = input$nudge_x,
          nudge_y = input$nudge_y,
          box.padding = input$box.padding,
          point.padding = input$point.padding,
          hjust = input$hjust,
          vjust = input$vjust,
          point.size = input$point.size,
          min.segment.length = input$min.segment.length,
          xlim = input$xlim,
          seed = input$seed
          ) +
        geom_point(color = 'red') +
        theme_classic(base_size = 16) +
        coord_equal()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
