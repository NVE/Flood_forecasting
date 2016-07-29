# library(leaflet)

single_station_map <- function(stations, selected_regine_main,
                               selected_name,
                               selected_long,
                               selected_lat) {
  
  map <- leaflet() %>% addTiles() %>%
    setView(13, 64, zoom = 5)  %>%
    addCircleMarkers(data = stations, lng = ~ long, lat = ~ lat, 
                     popup = paste("Name:", as.character(stations$name), "Number:", stations$regine_main,
                                   sep = " "), radius = 5, 
                     color = "black",  #  ~my.color.func(station$length_rec, my.colors), 
                     stroke = FALSE, fillOpacity = 0.5,
                     layerId = stations$regine_main) %>%
    addPopups(selected_long, selected_lat, paste("Name:", as.character(selected_name), "Number:", 
                                                 selected_regine_main, sep = " "),
              options = popupOptions(closeButton = FALSE, maxWidth = 100)) 
  
  #  %>%
  
  #     addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
  #               title = "Length of flood record (years)",
  #               opacity = 1)
  return(map)
}


multiple_station_map <- function(stations, selected_regine_main,
                                 selected_name,
                                 selected_long,
                                 selected_lat, with_popups = FALSE) {
  
  
  map <- leaflet() %>%
    addTiles() %>%
    setView(13, 64, zoom = 5)  %>%
    addCircleMarkers(data = stations, lng = ~ long, lat = ~ lat, 
                     popup = paste("Name:", as.character(stations$name), "Number:", stations$regine_main,
                                   sep = " "), radius = 5, 
                     color = "black",  #  ~my.color.func(station$length_rec, my.colors), 
                     stroke = FALSE, fillOpacity = 0.5,
                     layerId = stations$regine_main)   %>%
    addDrawToolbar(
                layerID = "selectbox",
                polyline = FALSE,
                circle = FALSE,
                marker = FALSE,
                edit = TRUE,
                polygon = TRUE,
                rectangle = TRUE,
                remove = TRUE,
                singleLayer = TRUE  # This allows only 1 polygon at a time when TRUE
              ) 
  
  if (!is.null(selected_regine_main) && with_popups == TRUE) {
    map <- map %>% addPopups(selected_long, selected_lat, paste("Name:", as.character(selected_name), "Number:", 
                                                 selected_regine_main, sep = " "),
              options = popupOptions(closeButton = FALSE, maxWidth = 100))
  }
  
  return(map)
}


which_station_in_polygon <- function(stations, map_selection) {
  
  if (!is.null(map_selection)) {
  
  temp <- as.matrix(map_selection)
  nb_points <- length(temp[ , 1]) - 1
  coord <- matrix(data = NA, nrow = nb_points, ncol = 2)
  
  for (i in 1:nb_points) {
    coord[i, ] <- c(map_selection[[i]][[1]],
                    map_selection[[i]][[2]])
  }

  # Apply point.in.polygon over all stations available
  is_in_poly <- rep(NA, length(stations$regine_main))
  stations_in_poly <- c()
  j <- 1
  
  for (i in seq(along = stations$regine_main)) {
    # integer array; values are: 
    # 0: point is strictly exterior to pol; 
    # 1: point is strictly interior to pol; 
    # 2: point lies on the relative interior of an edge of pol; 
    # 3: point is a vertex of pol.
    if (point.in.polygon(stations$long[i], stations$lat[i], coord[ , 1], coord[ , 2], mode.checked=FALSE) != 0) {
      is_in_poly[i] <- 1
      stations_in_poly[j] <- stations$regine_main[i]
      j <- j+1
    }
  }
  # Is there a way to "apply" this thing
  # test <- sapply(stations$regine_main, point.in.polygon, point.x = stations$long, point.y = stations$lat, pol.x = coord[ , 1], pol.y = coord[ , 2], mode.checked=FALSE)

  return(which(is_in_poly == 1))
  }
  
}

