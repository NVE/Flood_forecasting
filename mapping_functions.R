# library(leaflet)

# pal <- colorNumeric(
#   palette = heat.colors(5),
#   domain = c(0, 1/3, 2/3, 1, 4/3, 5/3))
# qpal <- colorQuantile("RdYlBu", length.bins, n = 5)

single_station_map <- function(stations, selected_nbname = NULL,
                               selected_long  = NULL,
                               selected_lat  = NULL, variable2plot = "none", map_layer = "open streetmap", catchments = FALSE, colored_markers = FALSE, radius_function = TRUE)  {
  
  ## Functions controling color and size of markers
  my.colors <- c("blue", "green", "yellow", "orange", "red", "black")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0, 1/3, 2/3, 1, 4/3, 5/3)
    color <- my.colors[trunc(x2plot * 3) + 2]
    invisible(color)
  }
 if (radius_function) {
   my.radius.func <- function(x2plot) {
     radius <- 4 * exp(x2plot * 2)
   }
 } else {
   my.radius.func <- function(x2plot) {
     radius <- 4
   }
 }
  
  ## Custom icon
#   custom_icon <- iconList(blue = makeIcon("./www/icon.png", iconWidth = 24, iconHeight =32),
#                          green = makeIcon("/Users/jazzurro/Documents/Stack Overflow/green.png", iconWidth = 24, iconHeight =32),
#                          orange = makeIcon("/Users/jazzurro/Documents/Stack Overflow/orange.png", iconWidth = 24, iconHeight =32))
  
  map <- leaflet() %>% setView(13, 64, zoom = 5)
    
  ## Color the markers to indicate potential flood issues
  if (variable2plot == "flood_warning") {
    
    index <- is.na(stations$flood_warning)
    NA_stations <- lapply(stations, function(x) x[index])
    OK_stations <- lapply(stations, function(x) x[!index])
    
    map <- map %>%
      addCircleMarkers(data = OK_stations, lng = ~ longitude, lat = ~ latitude, 
                       popup = paste(as.character(OK_stations$nbname),"Warning ratio:", round(OK_stations$flood_warning,2),
                                     sep = " "), radius = ~my.radius.func(OK_stations$flood_warning), 
                       color = ~my.color.func(OK_stations$flood_warning, my.colors), 
                       stroke = FALSE, fillOpacity = 1,
                       group = "Warning ratio available"
                       # layerId = OK_stations$regine_main
      ) %>%
      addCircleMarkers(data = NA_stations, lng = ~ longitude, lat = ~ latitude, 
                       popup = paste(as.character(NA_stations$nbname),"No warning ratio available",
                                     sep = " "), radius = 4, 
                       color = "blue", 
                       stroke = FALSE, fillOpacity = 1,
                       group = "No warning ratio"
                       # layerId = NA_stations$regine_main
      ) %>%
#       addLayersControl(position = "topright", options = layersControlOptions(collapsed = TRUE),
#                        # baseGroups = c("OpenStreetMap", "Esri.WorldImagery","Topo map"),
#                        overlayGroups = c("Warning ratio available", "No warning ratio")) %>%
      addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "4/3-5/3"),
                title = "Max forecast / mean annual flood",
                opacity = 1) %>%
      addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
                       popup = paste(as.character(stations$nbname),
                                     sep = " "), radius = ~my.radius.func(OK_stations$flood_warning), 
                       color = "white", weight = 0, stroke = TRUE,
                       fillOpacity = 0, fillColor = "white",
                       # group = "Selectable stations",
                       layerId = stations$regine_main)
  }
  
    if (variable2plot == "uncertainty")  {
  
      index <- is.na(stations$uncertainty)
      NA_stations_uncertainty <- lapply(stations, function(x) x[index])
      OK_stations_uncertainty <- lapply(stations, function(x) x[!index])
      
        map <- map %>%
          addCircleMarkers(data = OK_stations_uncertainty, lng = ~ longitude, lat = ~ latitude, 
                           popup = paste(as.character(OK_stations_uncertainty$nbname),"Warning ratio:", round(OK_stations_uncertainty$uncertainty,2),
                                         sep = " "), radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty) ), 
                           color = ~my.color.func(OK_stations_uncertainty$uncertainty, my.colors), 
                           stroke = FALSE, fillOpacity = 1,
                           group = "Uncertainty of HBV_2014"
                           # layerId = OK_stations_uncertainty$regine_main
          ) %>%
          addCircleMarkers(data = NA_stations_uncertainty, lng = ~ longitude, lat = ~ latitude, 
                           popup = paste(as.character(NA_stations_uncertainty$nbname),"No warning ratio available",
                                         sep = " "), radius = 4, 
                           color = "blue", 
                           stroke = FALSE, fillOpacity = 1,
                           group = "No uncertainty figure"
                           # layerId = NA_stations_uncertainty$regine_main
          ) %>%
          addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "4/3-5/3"),
                    title = "Max forecast / mean annual flood",
                    opacity = 1) %>%
          addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
                           popup = paste(as.character(stations$nbname),
                                         sep = " "), radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty) ), 
                           color = "white", weight = 0, stroke = TRUE,
                           fillOpacity = 0, fillColor = "white",
                           # group = "Selectable stations",
                           layerId = stations$regine_main)
        
    } 
  if (variable2plot == "none") {
    map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude, 
                           popup = paste(as.character(stations$nbname),
                                         sep = " "), radius = 5, 
                           color = "black", weight = 4, stroke = TRUE,
                           fillOpacity = 1, fillColor = "black",
                           # group = "Selectable stations",
                           layerId = stations$regine_main)
  }

  
    
#       addMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, icon = custom_icon[1],
#                        popup = paste(as.character(stations$nbname),
#                                      sep = " "), 
#                        layerId = stations$regine_main)
#       addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
#                        popup = paste(as.character(stations$nbname),
#                                      sep = " "), radius = 10, 
#                        color = "white", weight = 3, stroke = TRUE,
#                        fillOpacity = 0, fillColor = "white",
#                        # group = "Selectable stations",
#                        layerId = stations$regine_main)
      
      #     addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
#                      popup = paste(as.character(stations$nbname),
#                                    sep = " "), radius = 5, 
#                      color = "black", weight = 4, stroke = TRUE,
#                      fillOpacity = 0, fillColor = "white",
#                      # group = "Selectable stations",
#                      layerId = stations$regine_main)
  
    
  ## Add catchment boundaries to the map and change base layer
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
    map <- addProviderTiles(map, "Esri.WorldImagery", group = "Esri.WorldImagery") 
#       addWMSTiles(map, "http://wms.geonorge.no/skwms1/wms.nib",
#                        layers = "ortofoto",
#                        # "http://wms.geonorge.no/skwms1/wms.terrengmodell",
#                        # "http://openwms.statkart.no/skwms1/wms.terrengmodell",
#                         # layers = "terreng",  # did not work,
#                        # options = WMSTileOptions(format:"image/png", transparent = FALSE),
#                        tileOptions(tms = TRUE),
#                        attribution = "Kartverket")
  }
  if (map_layer == "open streetmap") {
    map <- addTiles(map)
  }

  return(map)
}


multiple_station_map <- function(stations, selected_regine_main = NULL,
                                 selected_name = NULL,
                                 selected_long = NULL,
                                 selected_lat = NULL, single_poly = FALSE) {
  
  map <- leaflet() %>%
    addTiles() %>%
    setView(13, 64, zoom = 5)  %>%
    addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
                     popup = paste("Name:", as.character(stations$name), "Number:", stations$regine_main,
                                   sep = " "), radius = 5, 
                     color = "black", 
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