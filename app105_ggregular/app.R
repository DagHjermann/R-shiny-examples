#
# Testing ggrepel options
# See 'https://ggrepel.slowkow.com/articles/examples.html'  
#

library(shiny)
library(ggplot2)
library(dplyr)

mtcars <- mtcars %>%
  mutate(mpg2 = mpg/6)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Testing ggrepel options"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("line_length", "line_length", 
                  min = 0, max = 50, step = 2, value = 10),
      sliderInput("angle", "angle", 
                  min = 0, max = 360, step = 5, value = 45)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("repel_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  xrange <- diff(range(mtcars$wt))
  aspect <- with(mtcars, diff(range(mpg))/diff(range(wt)))
  # line_length <- 10
  # angle <- 45
  angle_rad <- reactive(
    input$angle/180*pi
  )
  line_length_abs <- reactive({
    input$line_length/100*xrange
  })
  x_abs <- reactive({
    line_length_abs()*cos(angle_rad())
  })
  
  y_abs <- reactive(
    line_length_abs()*sin(angle_rad())*aspect
  )
  
  output$repel_plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
      geom_point(color = 'red') +
      geom_segment(aes(xend = wt + x_abs(), yend = mpg + y_abs())) +
      geom_text(aes(x = wt + x_abs(), y = mpg + y_abs(), label = rownames(mtcars)), hjust = 0) +
      theme_classic(base_size = 16)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
