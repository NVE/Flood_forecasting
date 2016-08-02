mapModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  print("proutMap")
  fluidRow(
    column(8, leafletOutput(ns("map")) ),
    column(4,
  selectInput(ns("station"), selected = "2.11", 
              label = "Choose a station", choices = stations_available)),
  column(4,
         radioButtons(ns("map_layer"), selected = "open streetmap", 
                     label = "Choose a map layer", choices = c("open streetmap", "topo map", "aerial"))
    )
  )
  
}

mapModule <- function(input, output, session) {
  # stations is global but gets send to the mapping function so that this function can be used in other settings!

  selected_regine_main <- reactive(input$station)
  selected_name <- reactive(stations$name[which(stations$regine_main == input$station)])
  selected_long <- reactive(stations$long[which(stations$regine_main == input$station)])
  selected_lat <-  reactive(stations$lat[which(stations$regine_main == input$station)])

  
  output$map <- renderLeaflet({single_station_map(stations, selected_regine_main(),
                                                  selected_name(),
                                                  selected_long(),
                                                  selected_lat(), input$map_layer)})
  
  # Interactivity of input between station selector and map
  observeEvent(input$map_marker_click, { # update the map markers and view on map clicks
    p <- input$map_marker_click
    leafletProxy("map")
    
    updateSelectInput(session, inputId='station', selected =  p$id, 
                      label = "Choose a station", choices = stations_available)
  })
  
  
  return(input)
  
}

mapModule_polygonFeature <- function(input, output, session) {
  

#   map <- reactive(multiple_station_map(stations, selected_regine_main(),
#                                        selected_name(), selected_long(), selected_lat()))

  # Get coordinates of the selected polygon
  map_selection <- reactive(input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]])
  # Reactive parameters of the stations inside the polygon
  selected_stations_indices <- reactive(which_station_in_polygon(stations, map_selection()))
  selected_regine_main <-      reactive(stations$regine_main[selected_stations_indices()])
  selected_name <-             reactive(stations$name[selected_stations_indices()])
  selected_long <-             reactive(stations$long[selected_stations_indices()])
  selected_lat <-              reactive(stations$lat[selected_stations_indices()])
  
  # Create map and update the color of the completed polygon to green
  map <- reactive(multiple_station_map(stations, selected_regine_main(),
                              selected_name(), selected_long(), selected_lat()) %>% 
                  addGeoJSON(input$map_selectbox_features, color="green"))
  
  output$map <- renderLeaflet( map()   ) 
                  
  output$print_selection <- renderText({ paste("-", selected_regine_main()) })
         
  # return(selected_regine_main)
}


mapModule_polygonFeatureUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
# fluidPage(
fluidRow(
  
      column(6, leafletOutput(ns("map")) ),
      column(6,
             wellPanel(h4('Select a group of stations with the map, using the polygon or rectangle tools')),
#              wellPanel(
#   selectInput(ns("model"), selected = "HBV_2014", 
#               label = "Choose a model", choices = c("HBV_2014", "HBV_2016", "DDD"))
#   ),
  wellPanel(
        h4('Selected stations'),    
        verbatimTextOutput(ns("print_selection"))
        )
  ))
}

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



########################################

#############################################################################################
# To be able to select stations directly on the map (for the first tab) 



# change the station selection on the first tab when a new station is selected in the rlevels tab

# observeEvent(input$station4rlevels, { 
#   updateSelectInput(session, inputId='station', selected = input$station4rlevels, 
#                     label = "Pick a station", choices = station$number)
# })
# # and accordingly change the station selection in the rlevels tab when a new station is selected in the main tab
# observeEvent(input$station, { 
#   updateSelectInput(session, inputId='station4rlevels', selected = input$station, 
#                     label = "Pick a station", choices = station$number)
# })

OLD_mapModule_polygonFeature <- function(input, output, session) {
  
  
  #   map <- reactive(multiple_station_map(stations, selected_regine_main(),
  #                                        selected_name(), selected_long(), selected_lat()))
  
  
  observe({
      
      # Get coordinates of the selected polygon
      map_selection <- input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]]
      
      selected_stations_indices <- which_station_in_polygon(stations, map_selection)
      selected_regine_main <- stations$regine_main[selected_stations_indices]
      selected_name <- stations$name[selected_stations_indices]
      selected_long <- stations$long[selected_stations_indices]
      selected_lat <-  stations$lat[selected_stations_indices]
      
      map <- multiple_station_map(stations, selected_regine_main,
                                  selected_name, selected_long, selected_lat)
      
      # change the color of the completed polygon to green
      output$map <- renderLeaflet( map %>% addGeoJSON(input$map_selectbox_features, color="green")  ) 
      
      # Check which stations are inside the polygon
      output$print_selection <- renderText({ paste("-", selected_regine_main) })
      
      # eval(as.symbol(input$model)) transforms a string input into the corresponding data
      # callModule(forecast_plot_mod2, "multi_station_plot", as.character(selected_regine_main), eval(as.symbol(input$model)))
      callModule(OLD_multimod_forecast_plot, "multi_station_plot", as.character(selected_regine_main), HBV_2014, HBV_2016, DDD, HBV_past_year,
                 input$variable_1, input$variable_2, input$variable_3, input$variable_4)
    })
  
}


OLD_mapModule_polygonFeatureUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      column(6, leafletOutput(ns("map")) ),
      column(6,
             wellPanel(h4('Select a group of stations with the map, using the polygon or rectangle tools')),
             wellPanel(
               h4('Selected stations'),    
               verbatimTextOutput(ns("print_selection"))
             ))),
    fluidRow(
      column(3,
               selectInput(ns("variable_1"), label = "Variables for HBV_2014", 
                           choices = unique(filter(HBV_2014, Type == "Runoff")$Variable), multiple = TRUE) ),
      column(3,
               selectInput(ns("variable_2"), label = "Variables for HBV_2016", 
                           choices = unique(filter(HBV_2016, Type == "Runoff")$Variable), multiple = TRUE) ),
      column(3,  
               selectInput(ns("variable_3"), label = "Variables for DDD", 
                           choices = unique(filter(DDD, Type == "Runoff")$Variable), multiple = TRUE) ),
      column(3,
               selectInput(ns("variable_4"), label = "Variables for HBV_past_year", 
                           choices = unique(filter(HBV_past_year, Type == "Runoff")$Variable), multiple = TRUE) )
             ),
    
    forecast_plot_modUI(ns("multi_station_plot"))
  )
}