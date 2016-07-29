mapModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(8, leafletOutput(ns("map")) ),
    column(4,
  selectInput(ns("station"), selected = "2.11", 
              label = "Choose a station", choices = stations_available)
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
                                                  selected_lat())})
  
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
 

  observeEvent({input$map_selectbox_features
                input$model}, {
 
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
                  
  callModule(forecast_plot_mod2, "multi_station_plot", as.character(selected_regine_main), eval(as.symbol(input$model)))
  })
  
}


mapModule_polygonFeatureUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
fluidPage(
fluidRow(
  
      column(6, leafletOutput(ns("map")) ),
      column(6,
             wellPanel(h4('Select a group of stations with the map, using the polygon or rectangle tools')),
             wellPanel(
  selectInput(ns("model"), selected = "HBV_2014", 
              label = "Choose a model", choices = c("HBV_2014", "HBV_2016", "DDD"))
  ),
  wellPanel(
        h4('Selected stations'),    
        verbatimTextOutput(ns("print_selection"))
        )
  )),
  
forecast_plot_modUI(ns("multi_station_plot"))
)
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






##############################################################################################


printoutModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  fluidRow(
    verbatimTextOutput(ns("msg"))  
  )
}


printoutModule  <- function(input, output, session, map_input, stations = NA) {
  
  # Now we need to add markers for the stations selected
  #   ydata <- reactive({
  #     filter(data, year == input$year)
  #   })
  
  msg <- "No polygon drawn yet"
  output$msg <- renderPrint({print(msg)})
  
  is_in_poly <- NA
  observeEvent(map_input$map_selectbox_features, {
    
    temp <- as.matrix(map_input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]])
    # print(length(temp[ , 1]))
    
    nb_points <- length(temp[ , 1]) - 1
    coord <- matrix(data = NA, nrow = nb_points, ncol = 2)
    for (i in 1:nb_points) {
      coord[i, ] <- c(map_input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]][[i]][[1]],
                      map_input$map_selectbox_features$features[[1]]$geometry$coordinates[[1]][[i]][[2]])
    }
    
    test_point <- coord[1, ]
    test_point2 <- coord[3, ]
    test_point <- (test_point + test_point2) / 2
    
    is_in_poly <- point.in.polygon(test_point[1], test_point[2], coord[ , 1], coord[ , 2], mode.checked=FALSE)
    
    switch(is_in_poly == 0, msg <- "point is strictly exterior to polygon")
    switch(is_in_poly == 1, msg <- "point is strictly interior to polygon")
    switch(is_in_poly == 2, msg <- "point lies on the relative interior of an edge of polygon")
    switch(is_in_poly == 3, msg <- "point is a vertex of polygon")
    
    output$msg <- renderPrint({print(msg)})
  })
  
}