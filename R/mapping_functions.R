# This file contains all the mapping functions developed for the Flomvarsling shiny app


#' Function that controls the colour of the colour markers
#' @param x2plot 
#' @param my.colors 
#' @return
#' @export
#' @examples my.colors <- c("blue", "green", "yellow", "orange", "red", "black")
#' my.color.func(OK_stations$flood_warning, my.colors)
my.color.func <- function(x2plot, my.colors) {
  color.bins <- c(0, 1/3, 2/3, 1, 4/3, 5/3)
  color <- my.colors[trunc(x2plot * 3) + 2]
  invisible(color)
}

#' Function that control the size of the colour markers
#' @param x2plot 
#' @param radius_function 
#'
#' @return
#' @export
#'
#' @examples map <- addCircleMarkers(map, data = OK_stations, lng = ~ longitude, lat = ~ latitude, 
#'                  radius = ~my.radius.func(OK_stations$flood_warning, radius_function), 
#'                  color = ~my.color.func(OK_stations$flood_warning, my.colors), 
#'                  stroke = FALSE, fillOpacity = 1,
#'                  group = "Warning ratio available")
my.radius.func <- function(x2plot, radius_function) {
  if (radius_function == TRUE) {
    radius <- 3 * exp(x2plot)
  }
  else {
    radius <- 3
  }
}

#' Sub-function use to find which stations are in the polygon the user has drawn
#' @param stations It is a global variable in the Shiny App but is included here as a parameter for general use. 
#' See global.R for the data preparation
#' @param map_selection 
#' @importFrom sp point.in.polygon
#' @return
#' @export
#'
#' @examples As used in the "mapModule_polygonFeature" server module:
#' selected_stations_indices <- reactive(which_station_in_polygon(stations, input$map_draw_all_features$features))
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
          # 0: point is strictly exterior to the polygon; 
          # 1: point is strictly interior tothe polygon; 
          # 2: point lies on the relative interior of an edge of the polygon; 
          # 3: point is a vertex of pol.
          if (point.in.polygon(stations$long[i], stations$lat[i], coord[ , 1], coord[ , 2], mode.checked=FALSE) != 0) {
            is_in_poly[i] <- 1
            stations_in_poly[j] <- stations$regine_main[i]
            j <- j+1
          }
        }
        # Is there a way to "apply" this thing?
        # test <- sapply(stations$regine_main, point.in.polygon, point.x = stations$long, point.y = stations$lat, pol.x = coord[ , 1], pol.y = coord[ , 2], mode.checked=FALSE)
        station_list <- c(station_list, which(is_in_poly == 1))
      }
    }
  }
  return(station_list)
}

#' Mapping function for the first tab of the flomvarsling ShinyApp. 
#' This function is called by the shiny server module "mapModule" 
#' @param stations It is a global variable in the Shiny App but is included here as a parameter for general use. 
#' See global.R for the data preparation
#' @param selected_nbname Selected station in the following format. "2.11-narsjø". This can be a vector
#' @param selected_long 
#' @param selected_lat 
#' @param variable2plot "Fare for flom", "Usikkerhet" or "Ingen"
#' @param map_layer "Openstreetmap" by default. Other choices: "Topo kart" or "Flyfoto"
#' @param catchments Specify TRUE if you want to see the catchment boundaries
#' @param radius_function Default "TRUE". Functions which makes marker size proportional to the variable2plot value
#' @param popups Specify TRUE if you want to see have popups for the selected stations
#' @import leaflet
#' @return map Returns a map that is used by "mapModule"
#' @export
#'
#' @examples This is how we use this function in the shiny server module "mapModule"
#' output$map <- renderLeaflet({station_map(stations, input$station, selected_long(),
#' selected_lat(), variable2plot = input$variable, map_layer = input$map_layer, catchments = input$catchments, popups = input$popups)})

station_map <- function(stations, selected_nbname = NULL,
                               selected_long  = NULL,
                               selected_lat  = NULL, variable2plot = "Ingen", map_layer = "Openstreetmap", catchments = FALSE, 
                               radius_function = TRUE, popups = FALSE)  {
  
  ## my.colors to be used with my.color.func in the mapping code
  my.colors <- c("blue", "green", "yellow", "orange", "red", "black")
  
map <- leaflet() %>% setView(13, 64, zoom = 5)
  
  ## Color the markers to indicate potential flood issues
  if (variable2plot == "Fare for flom") {
    
    index <- is.na(stations$flood_warning)
    NA_stations <- lapply(stations, function(x) x[index])
    OK_stations <- lapply(stations, function(x) x[!index])
    
    map <- map %>%
      addCircleMarkers(data = OK_stations, lng = ~ longitude, lat = ~ latitude, 
                       radius = ~my.radius.func(OK_stations$flood_warning, radius_function), 
                       color = ~my.color.func(OK_stations$flood_warning, my.colors), 
                       stroke = FALSE, fillOpacity = 1,
                       group = "Warning ratio available") %>%
      addCircleMarkers(data = NA_stations, lng = ~ longitude, lat = ~ latitude, 
                       radius = 3, 
                       color = "blue", 
                       stroke = FALSE, fillOpacity = 1,
                       group = "No warning ratio") %>%
      addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "> 4/3"),
                title = "Verdien av farge markoer",
                opacity = 1)
    
    # Adding transparent markers with layerID = selected stations so that the map interactivity remains
    if (popups == FALSE) {
      map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude, 
                              popup = paste(as.character(stations$nbname), "Ratio:", round(stations$flood_warning,2), sep = " "),
                              radius = ~my.radius.func(stations$flood_warning, radius_function), 
                              color = "white", weight = 0, stroke = TRUE,
                              fillOpacity = 0, fillColor = "white",
                              layerId = stations$nbname)
    } else {
      map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude, 
                              radius = ~my.radius.func(stations$flood_warning, radius_function), 
                              color = "white", weight = 0, stroke = TRUE,
                              fillOpacity = 0, fillColor = "white",
                              layerId = stations$nbname)
    }
  }
  
  if (variable2plot == "Usikkerhet")  {

    index <- is.na(stations$uncertainty)
    NA_stations_uncertainty <- lapply(stations, function(x) x[index])
    OK_stations_uncertainty <- lapply(stations, function(x) x[!index])


    map <- map %>%
      addCircleMarkers(data = OK_stations_uncertainty, lng = ~ longitude, lat = ~ latitude,
                       radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty), radius_function),
                       color = ~my.color.func(OK_stations_uncertainty$uncertainty, my.colors),
                       stroke = FALSE, fillOpacity = 1,
                       group = "Uncertainty of HBV_2014") %>%
      addCircleMarkers(data = NA_stations_uncertainty, lng = ~ longitude, lat = ~ latitude,
                       radius = 3,
                       color = "blue",
                       stroke = FALSE, fillOpacity = 1,
                       group = "No uncertainty figure") %>%
      addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "> 4/3"),
                title = "Verdien av farge markoer",
                opacity = 1)

    if (popups == FALSE) {
      map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude,
                              popup = paste(as.character(stations$nbname), "Ratio:", round(OK_stations_uncertainty$uncertainty,2), sep = " "),
                              radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty), radius_function),
                              color = "white", weight = 0, stroke = TRUE,
                              fillOpacity = 0, fillColor = "white",
                              layerId = stations$nbname)
    } else {
      map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude,
                              # popup = paste(as.character(stations$nbname), "Ratio:", round(OK_stations$uncertainty,2), sep = " "),
                              radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty), radius_function),
                              color = "white", weight = 0, stroke = TRUE,
                              fillOpacity = 0, fillColor = "white",
                              layerId = stations$nbname)

    }
  }

  if (variable2plot == "Ingen") {
    map <- addCircleMarkers(map, data = stations, lng = ~ longitude, lat = ~ latitude,
                            popup = paste(as.character(stations$nbname), sep = " "),
                            radius = 5,
                            color = "black", weight = 4, stroke = TRUE,
                            fillOpacity = 1, fillColor = "black",
                            layerId = stations$nbname)
  }
  
  
  
  ## Add catchment boundaries to the map and change base layer
  if (catchments == TRUE) {
    map <- addGeoJSON(map, hbv_catchments, weight = 3, color = "#444444", fill = FALSE)
  }
  if (map_layer == "Topo kart") {
    map <- addWMSTiles(map,
                       "http://wms.geonorge.no/skwms1/wms.topo2",
                       layers = "topo2_WMS",
                       options = WMSTileOptions(format = "image/png", transparent = TRUE),
                       tileOptions(tms = TRUE),
                       attribution = "Kartverket")
  }
  if (map_layer == "Flyfoto") {
    map <- addProviderTiles(map, "Esri.WorldImagery", group = "Esri.WorldImagery") 
  }
  if (map_layer == "Openstreetmap") {
    map <- addTiles(map)
  }
  
  return(map)
}


#' Mapping function used by the server module "mapModule_polygonFeature"
#' @param stations It is a global variable in the Shiny App but is included here as a parameter for general use. 
#' See global.R for the data preparation
#' @param selected_regine_main 
#' @param selected_name 
#' @param selected_long 
#' @param selected_lat 
#' @param single_poly 
#' @param variable2plot 
#' @param radius_function 
#' @param popups 
#' @import leaflet
#' @import leaflet.extras
#' @return map Returns a leaflet map to the "mapModule_polygonFeature" server module
#' @export
#'
#' @examples See use in the "mapModule_polygonFeature" module
#' output$map <- renderLeaflet(multiple_station_map(stations, single_poly = FALSE, variable2plot = input$variable, popups = input$popups))
multiple_station_map <- function(stations, selected_regine_main = NULL,
                                 selected_name = NULL,
                                 selected_long = NULL,
                                 selected_lat = NULL, single_poly = FALSE, variable2plot = "Ingen", radius_function = TRUE, popups = FALSE) {

  map <- leaflet() %>%
    addTiles() %>%
    setView(13, 64, zoom = 5)

# Works with the leaflet + leaflet.extras
      map <- addDrawToolbar(map,
                            # targetLayerId = "draw",
                            targetGroup='draw',
                            circleOptions = FALSE,
                            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
                            singleFeature = FALSE)  %>%
        
    addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
                     popup = paste("Name:", as.character(stations$name), "Number:", stations$regine_main,
                                   sep = " "), radius = 5,
                     color = "black", weight = 4, stroke = TRUE,
                     fillOpacity = 1, fillColor = "black",
                     layerId = stations$nbname)
  
  return(map)
}
