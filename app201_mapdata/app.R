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
#   selected_station_code (reactive values from click)
#   output$map internals:
#     stations_preselected <- stations_for_map (filter: input$param and input$species)
#     stations_for_map_selected <- stations_for_map (filter: selected_station_code)

# Lookup data for species
lookup_species <- data.frame(
  species = c("Cod / torsk", "Blue mussel / blåskjell", "Dog whelk / purpursnegl", "Eider duck / ærfugl"),
  LATIN_NAME = c("Gadus morhua", "Mytilus edulis", "Nucella lapillus", "Somateria mollissima")
)
# Read data
data_all <- read.csv("../input_data/milkys_example.csv") %>% 
  left_join(lookup_species, by = join_by(LATIN_NAME))
params_all <- unique(data_all$PARAM) %>% sort()

# Lookup data for stations
stations <- read.csv("../input_data/milkys_example_coord.csv") %>%
  rename(x = Long, y = Lat)  %>%
  filter(STATION_CODE %in% unique(data_all$STATION_CODE))

stations_for_map <- data_all %>%
  distinct(STATION_CODE, LATIN_NAME, species) %>%
  left_join(stations, by = join_by(STATION_CODE)) %>%
  mutate(
    color = case_when(
      LATIN_NAME %in% "Gadus morhua" ~ "red",
      LATIN_NAME %in% "Mytilus edulis" ~ "blue",
      LATIN_NAME %in% "Nucella lapillus" ~ "yellow",
      LATIN_NAME %in% "Somateria mollissima" ~ "purple")
  )

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
         shiny::checkboxGroupInput(inputId = "species", "Species", species_all, "Cod / torsk"),
         shiny::checkboxInput(inputId = "choose_from_map", "Choose station from map", value = FALSE),
         shiny::uiOutput("station"),
         # shiny::selectizeInput(),
         # should add interactive stations selector, which is updated when the map is clicked
         # shiny::selectizeInput("station", "Station", stations_all, "30B", multiple = FALSE),
         plotOutput("plot", height="300px")),
  br()
)

server <- function(input, output) {
  
  # build data with 2 places
  # stations = data.frame(x=c(130, 128), y=c(-22,-26), id=c("place1", "place2"))
  
  stations_preselected <- reactive({
    data_all %>%
      filter(
        PARAM %in% input$param,
        species %in% input$species) %>% 
      distinct(STATION_CODE) %>% 
      pull(STATION_CODE)
  })
  
  # Make station menu
  output$station <- renderUI({
    shiny::selectInput("station", "Station", stations_preselected(), selected_station_code())
  })
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # selected station code is either chosen by clicking on the map
  # - in which case station menu is governed by clicked station
  # or from the menu
  selected_station_code <- reactive({
    if (input$choose_from_map){
      # select stations that have been clicked (layer below top layer)
      result = data_of_click$clickedMarker$id
      if (is.null(result)){ result <- "30B" }
    } else {
      result = input$station
    }
    result
  })
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    # browser()
    # select stations by selected species (top layer of map)
    stations_for_map_show <- stations_for_map %>% 
      filter(STATION_CODE %in% stations_preselected())
    stations_for_map_selected <- stations_for_map %>%
      filter(STATION_CODE %in% selected_station_code())
    # show map
    leaflet() %>% 
      setView(lng = 13 , lat = 66, zoom = 4) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=stations_for_map_selected, ~x , ~y, 
                       radius=12, color="green",  fillColor="green", stroke = TRUE, weight = 1) %>%
      addCircleMarkers(data = stations_for_map_show, ~x , ~y, layerId=~STATION_CODE, popup=~STATION_CODE, 
                       fillColor=~color, 
                       radius=6 , color="black", stroke = TRUE, fillOpacity = 0.8, weight = 3)
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

