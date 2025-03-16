library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

# before app starts
#   data_all <- file 
#   stations <- file  
#   lookup_species <- hard-coded
#   stations_for_map <- data_all + stations + lookup_species   
#   params_all <- data_all
#   species_all <- stations_for_map -> 
#   stations_all <- stations_for_map
# ui uses 
#   params_all
#   species_all
# server
#   output$map internals:
#     stations_selected_species <- stations_for_map (filter: input$species)
#     selected_station_code (from click)
#     stations_for_map_selected <- stations_for_map (filter: selected_station)

# Read data
data_all <- read.csv("../input_data/milkys_example.csv")
stations <- read.csv("../input_data/milkys_example_coord.csv") %>%
  rename(x = Long, y = Lat)  %>%
  filter(STATION_CODE %in% unique(data_all$STATION_CODE))
params_all <- unique(data_all$PARAM) %>% sort()

lookup_species <- data.frame(
  species = c("Cod", "Blue mussel"),
  LATIN_NAME = c("Gadus morhua", "Mytilus edulis")
)

stations_for_map <- data_all %>%
  distinct(STATION_CODE, LATIN_NAME) %>%
  left_join(stations, by = join_by(STATION_CODE)) %>%
  left_join(lookup_species, by = join_by(LATIN_NAME))

species_all <- unique(stations_for_map$species)
stations_all <- unique(stations_for_map$STATION_CODE)

# from
# https://www.r-bloggers.com/2017/03/4-tricks-for-working-with-r-leaflet-and-shiny/

ui <- fluidPage(
  br(),
  column(8,
         leafletOutput("map", height="600px")),
  column(4, 
         br(),br(),br(),br(),
         shiny::selectInput("param", "Substance", params_all, "CD"),
         shiny::selectizeInput("species", "Species", species_all, "Cod", multiple = TRUE),
         # should add interactive stations selector, which is updated when the map is clicked
         # shiny::selectizeInput("station", "Station", stations_all, "30B", multiple = FALSE),
         plotOutput("plot", height="300px")),
  br()
)

server <- function(input, output) {
  
  # build data with 2 places
  # stations = data.frame(x=c(130, 128), y=c(-22,-26), id=c("place1", "place2"))
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  selected_station_code <- reactive({
    # select stations that have been clicked (layer below top layer)
    result = data_of_click$clickedMarker$id
    if(is.null(result)){
      result <- "30B"}
    result
  })
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    # select stations by selected species (top layer of map)
    stations_selected_species <- stations_for_map %>%
      filter(species %in% input$species)%>%
      mutate(
        color = case_when(
          species %in% "Cod" ~ "red",
          species %in% "Blue mussel" ~ "blue")
      )
    stations_for_map_selected <- stations_for_map %>%
      filter(STATION_CODE %in% selected_station_code())
    # show map
    leaflet() %>% 
      setView(lng = 13 , lat = 66, zoom = 4) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=stations_for_map_selected, ~x , ~y, 
                       radius=10, color="green",  fillColor="green", stroke = TRUE, weight = 1) %>%
      addCircleMarkers(data=stations_selected_species, ~x , ~y, layerId=~STATION_CODE, popup=~STATION_CODE, 
                       fillColor=~color, 
                       radius=6 , color="black", stroke = TRUE, fillOpacity = 0.8, weight = 3)
  })
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlot({
    data_selected <- data_all %>% 
      filter(STATION_CODE %in% selected_station_code() & PARAM %in% input$param)
    # plot(concentration~MYEAR, data = data_selected, main = paste("Station", selected_station_code()))
    ggplot(data_selected, aes(x = MYEAR, y = concentration)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(
        title = paste("Measurements for station:", selected_station_code()),
        x = "Year",
        y = "Concentration"
      )
  })
}

shinyApp(ui = ui, server = server)

