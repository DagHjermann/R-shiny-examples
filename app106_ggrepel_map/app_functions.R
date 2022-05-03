
#
# General function for transformation 
#
# Adds coordinates x and y
add_transformed_coord <- function(data, name_x = "long", name_y = "lat",
                                  from = "longlat", from_zone = NA, 
                                  to = "utm", to_zone = 32){
  
  data <- data %>%
    mutate(x = as.numeric(NA), y = as.numeric(NA)) %>%
    add_transformed_coord()
  
  # Define crs strings for conversion between long/lat and UTM
  crs_from <- crs_string(from, from_zone)
  crs_to <- crs_string(to, to_zone)
  
  coordinate_exists <- !is.na(data[[name_x]])   # sp doesn't like NAs
  sp_original <- SpatialPoints(data[coordinate_exists, c(name_x, name_y)],
                               proj4string = CRS(crs_from)
  )
  sp_transformed <- spTransform(sp_original, CRS(crs_to))
  
  # Add transformed coords to data set
  data$x[coordinate_exists] <- sp_transformed@coords[,1]
  data$y[coordinate_exists] <- sp_transformed@coords[,2]
  
  data
  
}