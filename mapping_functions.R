# library(leaflet)

########################################

#   pal <- colorNumeric(
#     palette = heat.colors(5),
#     domain = c(0,30,60,90,120,150))
# qpal <- colorQuantile("RdYlBu", length.bins, n = 5)

#   my.colors <- c("black", "red", "orange", "green", "blue")
#   
#   my.color.func <- function(x2plot, my.colors) {
#     color.bins <- c(0,30,60,90,120,150)
#     color <- my.colors[trunc(x2plot/30)+1]
#     invisible(color)
#   }

single_station_map <- function(stations, selected_nbname = NULL,
                               selected_long  = NULL,
                               selected_lat  = NULL, map_layer = "open streetmap", catchments = FALSE) {
  
  map <- leaflet() %>%
    setView(13, 64, zoom = 5)  %>%
    addCircleMarkers(data = stations, lng = ~ long, lat = ~ lat, 
                     popup = paste(as.character(stations$nbname),
                                   sep = " "), radius = 5, 
                     color = "black",  #  ~my.color.func(station$length_rec, my.colors), 
                     stroke = FALSE, fillOpacity = 0.5,
                     layerId = stations$regine_main) %>%
    addPopups(selected_long, selected_lat, paste(selected_nbname),
              options = popupOptions(closeButton = FALSE, maxWidth = 100)) 
  
  if (catchments == TRUE) {
    map <- addGeoJSON(map, hbv_catchments, weight = 3, color = "#444444", fill = FALSE)
  }
  if (map_layer == "topo map") {
    map <- addWMSTiles(map,
    "http://wms.geonorge.no/skwms1/wms.topo2",
    layers = "topo2_WMS",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    tileOptions(tms = TRUE),
    attribution = "Kartverket")
  }
  if (map_layer == "aerial") {
    map <- addWMSTiles(map,
                       "http://wms.geonorge.no/skwms1/wms.nib",
                       layers = "ortofoto",
                       
                       # "http://wms.geonorge.no/skwms1/wms.terrengmodell",
                       # "http://openwms.statkart.no/skwms1/wms.terrengmodell",
                        # layers = "terreng",  # did not work,
                       
                       # options = WMSTileOptions(format:"image/png", transparent = FALSE),
                       tileOptions(tms = TRUE),
                       attribution = "Kartverket")
  }
  if (map_layer == "open streetmap") {
    map <- addTiles(map)
  }

  #     addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
  #               title = "Length of flood record (years)",
  #               opacity = 1)
  return(map)
}


multiple_station_map <- function(stations, selected_regine_main = NULL,
                                 selected_name = NULL,
                                 selected_long = NULL,
                                 selected_lat = NULL, single_poly = FALSE) {
  
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
                singleLayer = single_poly  # This allows only 1 polygon at a time when TRUE
              ) 
  return(map)
}


which_station_in_polygon <- function(stations, map_selection) {
  
  nb_poly <- length(map_selection)
  station_list <- c()
  if (nb_poly == 0) {
    station_list <- NULL
  } else {
  for (p in 1:nb_poly) {
  if (!is.null(map_selection[[p]])) {
    
    nb_points <- length(map_selection[[p]]$geometry$coordinates[[1]]) - 1
    coord <- matrix(data = NA, nrow = nb_points, ncol = 2)
    
    for (i in 1:nb_points) {
      coord[i, ] <- c(map_selection[[p]]$geometry$coordinates[[1]][[i]][[1]],
                      map_selection[[p]]$geometry$coordinates[[1]][[i]][[2]])
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
    station_list <- c(station_list, which(is_in_poly == 1))
  }
  }
  }
    return(station_list)
}