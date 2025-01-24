
# example from
# https://rstudio.github.io/r2d3/articles/shiny.html

library(shiny)
library(r2d3)

ui <- fluidPage(
  inputPanel(
    sliderInput("bar_max", label = "Max:",
                min = 0, max = 1, value = 1, step = 0.05)
  ),
  d3Output("d3")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    # df <- data.frame(y = runif(5, 0, input$bar_max))
    y = runif(5, 0, input$bar_max)
    r2d3(
      y,
      script = system.file("examples/baranims.js", package = "r2d3")
    )
  })
}

shinyApp(ui = ui, server = server)

