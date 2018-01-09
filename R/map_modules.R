# This file contains all the mapping modules developed for the Flomvarsling shiny app
# NOTE: eval(as.symbol(input$model)) transforms a string input into the corresponding data

#' Shiny UI module to be used with the server module "mapModule"
#' @param id 
#' @param multiple_choice 
#' @import leaflet
#' @import shiny
#' @return NULL This is a UI module which returns a map, a selection buttons: station name, map layer, catchment boundaries,
#' pops ups for selected stations and a choice of color markers.
#' @export
#'
#' @examples See server.R and UI.R for its implementation into the shiny App
mapModuleUI <- function(id, multiple_choice = FALSE) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    column(6, leafletOutput(ns("map")) ),
    column(4,
           selectInput(ns("station"), selected = station_nbname[1], 
                       label = "Velg stasjon", choices = station_nbname, multiple = multiple_choice)),
    column(2,
           radioButtons(ns("map_layer"), selected = "Openstreetmap", 
                        label = "Velg kartlag", choices = c("Openstreetmap", "Topo kart", "Flyfoto"))
    ),
    column(2,
           checkboxInput(ns("catchments"), "Vis nedboerfelt", FALSE)
    ),
    column(2,
           checkboxInput(ns("popups"), "Pop-ups for valgte stasjoner", FALSE)
    ),
    column(2,
           radioButtons(ns("variable"), selected = "Ingen", 
                        label = "Velg fargemarkoer", choices = c("Ingen", "Fare for flom", "Usikkerhet"))
    )
  )
}

#' Shiny server module to be used with the UI module "mapModuleUI"
#' @param input 
#' @param output 
#' @param session 
#'
#' @return input This is a server module which returns the "input" from the selection buttonsto the UI module "mapModuleUI"
#' @export
#'
#' @examples See server.R and UI.R for its implementation into the shiny App
mapModule <- function(input, output, session) {
  # stations is global but gets send to the mapping function so that this function can be used in other settings!
  
  selected_long <- reactive(stations$long[which(station_nbname %in% input$station)])
  selected_lat <-  reactive(stations$lat[which(station_nbname %in% input$station)])
  
  output$map <- renderLeaflet({station_map(stations, input$station,  # "station_map" is in mapping_functions.R
                                                  selected_long(),
                                                  selected_lat(), variable2plot = input$variable, map_layer = input$map_layer, catchments = input$catchments, popups = input$popups)})
  
  # The proxy enables modifying the map after having called the function "station_map"
  # This is useful for adapting the map to user on/off selections
  ns <- session$ns
  proxy <- leafletProxy(ns("map"), session)  
  
  
  # Interactivity of input between station selector and map
  observeEvent(input$map_marker_click, { # update the map markers and view on map clicks
    p <- input$map_marker_click
    
    updateSelectInput(session, inputId='station', selected =  c(input$station, p$id), # station_nbname[which(station_numbers %in% p$id)]
                      label = "Velg stasjon", choices = station_nbname)
  })
  
  observe({
    if (length(selected_long()) > 0 && input$popups == TRUE) {
      proxy %>% clearPopups()
      for (i in seq(along = selected_long())) {
        station_index <- which(stations$nbname ==  input$station[i])
        long <- stations$longitude[station_index]
        lat <- stations$latitude[station_index]
        
        if (input$variable == "Fare for flom") {
          proxy %>% addPopups(long, lat, paste(input$station[i], "Ratio:", round(stations$flood_warning[station_index],2), sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
        }
        else if (input$variable == "Usikkerhet") {
          proxy %>% addPopups(long, lat, paste(input$station[i], "Ratio:", round(stations$uncertainty[station_index],2), sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
          
        } else {
          
          proxy %>% addPopups(long, lat, paste(input$station[i], sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
        }
      }
      
    } else {proxy %>% clearPopups()}
  })
  return(input)
}

#' Shiny UI module to be used with the server module "mapModule_polygonFeature"
#' @param id 
#'
#' @return NULL This is a UI module which returns a map, a selection buttons: station name, map layer, catchment boundaries,
#' pops ups for selected stations and a choice of color markers.
#' @export
#'
#' @examples See server.R and UI.R for its implementation into the shiny App
mapModule_polygonFeatureUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      column(6, leafletOutput(ns("map")) ),
      column(6,
             wellPanel(h4('Velg en gruppe av stasjoner med kartet, ved hjelp av polygon eller rektangel verktoey')),
             wellPanel(
               h4('Utvalgte stasjoner'),    
               verbatimTextOutput(ns("print_selection"))
             )),
      column(3,
             checkboxInput(ns("catchments"), "Vis nedboerfelt", FALSE)
      ),
      column(3,
             checkboxInput(ns("popups"), "Pop-ups for valgte stasjoner", FALSE)
      ),
      
      column(2,
             radioButtons(ns("variable"), selected = "Ingen", 
                          label = "Velg fargemarkoer", choices = c("Ingen", "Fare for flom", "Usikkerhet")))
      
    ),
    fluidRow(
      column(2,
             selectInput(ns("variable_1"), label = "HBV med usikkerhetmodell", 
                         choices = unique(filter(HBV_2014, Type == "Runoff")$Variable), selected  = c("HBV.UM.sim", "HBV.UM.korr", "Lo50", "Hi50"),
                         multiple = TRUE) ),
      column(2,
             selectInput(ns("variable_2"), label = "HBV med +/- 50% nedboer", 
                         choices = unique(filter(HBV_2016, Type == "Runoff")$Variable), selected  = c("HBV.P.sim", "HBV.P.korr", "P.p50"),
                         multiple = TRUE) ),
      column(2,  
             selectInput(ns("variable_3"), label = "DDD", 
                         choices = unique(filter(DDD, Type == "Runoff")$Variable), selected = c("DDD-Sim"),
                         multiple = TRUE) ),
      column(2,
             selectInput(ns("variable_4"), label = "Simuleringer siste ar",
                         choices = unique(filter(HBV_past_year, Type == "Runoff")$Variable), multiple = TRUE) ),
      column(2,
             selectInput(ns("type_rl"), label = "Gjentaksintervallgrunnlag", 
                         choices = unique(filter(flomtabell)$Type), multiple = TRUE) )),
    forecast_plot_modUI(ns("multi_station_plot"))
  )
}

#' Shiny server module to be used with the UI module "mapModule_polygonFeatureUI"
#' @param input 
#' @param output 
#' @param session 
#'
#' @return NULL This is a server module and could return "input" as with "mapModule", but I had some problems implementing it in a similar way.
#' So the plotting is integrated within this module with "poly_multimod_forecast_plot_mod" which makes things a bit confusing.
#' This implementation could probably be simplified to resemble more "mapModule".
#' @export
#'
#' @examples See server.R and UI.R for its implementation into the shiny App
mapModule_polygonFeature <- function(input, output, session) {
  # stations is global but gets send to the mapping function so that this function can be used in other settings!
  
  output$map <- renderLeaflet(multiple_station_map(stations, single_poly = FALSE, variable2plot = input$variable, popups = input$popups))
  
  ns <- session$ns
  proxy <- leafletProxy(ns("map"), session) 

  ## my.colors to be used with my.color.func in the mapping code
  my.colors <- c("blue", "green", "yellow", "orange", "red", "black")
  
  radius_function <- TRUE

  observeEvent(input$variable, {
  
               if (input$variable == "Fare for flom") {
                 
                 index <- is.na(stations$flood_warning)
                 NA_stations <- lapply(stations, function(x) x[index])
                 OK_stations <- lapply(stations, function(x) x[!index])
                 
                 proxy <- proxy %>%
                   clearControls() %>%
                   clearMarkers() %>%
                   # removeMarker(layerId = c("uncertainty1", "uncertainty2")) %>%
                   addCircleMarkers(data = OK_stations, lng = ~ longitude, lat = ~ latitude, 
                                    radius = ~my.radius.func(OK_stations$flood_warning, radius_function), 
                                    color = ~my.color.func(OK_stations$flood_warning, my.colors), 
                                    stroke = FALSE, fillOpacity = 1,
                                    group = "Warning ratio available"
                                    # layerId = "flood_warning1"
                   ) %>%
                   addCircleMarkers(data = NA_stations, lng = ~ longitude, lat = ~ latitude, 
                                    radius = 4, 
                                    color = "blue", 
                                    stroke = FALSE, fillOpacity = 1,
                                    group = "No warning ratio"
                                    # layerId = "flood_warning2"
                   ) %>%
                   addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "> 4/3"),
                             title = "Verdien av farge markoer",
                             opacity = 1)
                 
                 # Adding transparent markers with layerID = selected stations so that the map interactivity remains
                 # if (popups == FALSE) {
                 proxy <- addCircleMarkers(proxy, data = stations, lng = ~ longitude, lat = ~ latitude, 
                                         popup = paste(as.character(stations$nbname), "Ratio:", round(OK_stations$flood_warning,2), sep = " "), 
                                         radius = ~my.radius.func(OK_stations$flood_warning, radius_function), 
                                         color = "white", weight = 0, stroke = TRUE,
                                         fillOpacity = 0, fillColor = "white",
                                         # group = "Selectable stations",
                                         layerId = stations$nbname)
               }
               
               if (input$variable == "Usikkerhet")  {
                 
                 index <- is.na(stations$uncertainty)
                 NA_stations_uncertainty <- lapply(stations, function(x) x[index])
                 OK_stations_uncertainty <- lapply(stations, function(x) x[!index])
                 
                 proxy <- proxy %>%
                   clearControls() %>%
                   clearMarkers() %>%
                   # removeMarker(layerId = c("flood_warning1", "flood_warning2")) %>%
                   addCircleMarkers(data = OK_stations_uncertainty, lng = ~ longitude, lat = ~ latitude, 
                                    radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty), radius_function), 
                                    color = ~my.color.func(OK_stations_uncertainty$uncertainty, my.colors), 
                                    stroke = FALSE, fillOpacity = 1,
                                    group = "Uncertainty of HBV_2014"
                                    # layerId = "uncertainty1"
                   ) %>%
                   addCircleMarkers(data = NA_stations_uncertainty, lng = ~ longitude, lat = ~ latitude, 
                                    radius = 4, 
                                    color = "blue", 
                                    stroke = FALSE, fillOpacity = 1,
                                    group = "No uncertainty figure"
                                    # layerId = "uncertainty1"
                   ) %>%
                   addLegend(position = "bottomright", colors = my.colors, labels = c("NA", "0-1/3", "1/3-2/3", "2/3-1", "1-4/3", "> 4/3"),
                             title = "Verdien av farge markoer",
                             opacity = 1)
                 
                 # if (popups == FALSE) {
                 proxy <- addCircleMarkers(proxy, data = stations, lng = ~ longitude, lat = ~ latitude, 
                                         popup = paste(as.character(stations$nbname), "Ratio:", round(OK_stations_uncertainty$uncertainty,2), sep = " "), 
                                         radius = ~my.radius.func(OK_stations_uncertainty$uncertainty / max(OK_stations_uncertainty$uncertainty), radius_function), 
                                         color = "white", weight = 0, stroke = TRUE,
                                         fillOpacity = 0, fillColor = "white",
                                         # group = "Selectable stations",
                                         layerId = stations$nbname)  
               }
               
               if (input$variable == "Ingen") {
                 proxy <- proxy %>% 
                          clearControls() %>%
                          clearMarkers() %>%
                          addCircleMarkers(data = stations, lng = ~ longitude, lat = ~ latitude, 
                                         popup = paste(as.character(stations$nbname), sep = " "),
                                         radius = 5, 
                                         color = "black", weight = 4, stroke = TRUE,
                                         fillOpacity = 1, fillColor = "black",
                                         # group = "Selectable stations",
                                         layerId = stations$nbname)
               }
  })
  

  # Check which stations are inside the polygon
  selected_stations_indices <- reactive(which_station_in_polygon(stations, input$map_draw_all_features$features))
  selected_regine_main <- reactive(stations$regine_main[selected_stations_indices()])
  selected_name <- reactive(stations$name[selected_stations_indices()])
  selected_long <- reactive(stations$long[selected_stations_indices()])
  selected_lat <-  reactive(stations$lat[selected_stations_indices()])
  
  observeEvent(input$catchments, {
    if (input$catchments == TRUE) {
      proxy %>% addGeoJSON(hbv_catchments, weight = 3, color = "#444444", fill = FALSE)
    } else {proxy %>% clearGeoJSON()}
  })
  
  output$print_selection <- renderText({ "Velg stasjoner med karttegneverktoey: 
Du kan tegne flere polygoner / rektangler. 
Du kan slette dem for a endre ditt valg." })
  
  observeEvent({input$variable_1
    input$variable_2
    input$variable_3
    input$variable_4
    input$type_rl
    input$map_draw_all_features$features}, {
      callModule(poly_multimod_forecast_plot_mod, "multi_station_plot", as.character(selected_regine_main()), HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell,
                 input$variable_1, input$variable_2, input$variable_3, input$variable_4, input$type_rl)
  
      # To do the simpler call below I would need to come back to the initial attempt at the polygon map module output/input relationship. Would be better...    
      # callModule(multimod_forecast_plot_mod,"multi_station_plot", as.character(selected_regine_main()), OBS, HBV_2014, HBV_2016, DDD, HBV_past_year, flomtabell)

      output$print_selection <- renderText( paste("-", selected_regine_main()) )
    })
  
  
  observe({
    if (length(selected_stations_indices()) > 0 && input$popups == TRUE) {
      for (i in selected_regine_main()) {
        station_index <- which(stations$regine_main ==  i)
        long <- stations$longitude[station_index]
        lat <- stations$latitude[station_index]
        
        if (input$variable == "Fare for flom") {
          proxy %>% addPopups(long, lat, paste(i, "Ratio:", round(stations$flood_warning[station_index],2), sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
        }
        else if (input$variable == "usikkerhet") {
          proxy %>% addPopups(long, lat, paste(i, "Ratio:", round(stations$uncertainty[station_index],2), sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
          
        } else {
          
          proxy %>% addPopups(long, lat, paste(i, sep = " "),
                              options = popupOptions(closeButton = FALSE, maxWidth = 100))
        }
      }
      
    }
    else {proxy %>% clearPopups()}
  })
  
}