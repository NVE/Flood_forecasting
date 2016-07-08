mapModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # fluidRow(
  leafletOutput(ns("map"))
  # )
  
}

mapModule <- function(input, output, session) {
  
  myMap <- leaflet() %>%
    addTiles() %>%
    addDrawToolbar(
      layerID = "selectbox",
      polyline = FALSE,
      circle = FALSE,
      marker = FALSE,
      edit = FALSE,
      polygon = TRUE,
      rectangle = TRUE,
      remove = TRUE,
      singleLayer = TRUE)  # This allows only 1 polygon at a time when TRUE
  
  
  output$map <- renderLeaflet({myMap})
  
}

# Module server function
mapModule_polygonFeature <- function(input, output, session) {
  
  myMap <- leaflet() %>%
    addTiles() %>%
    addDrawToolbar(
      layerID = "selectbox",
      polyline = FALSE,
      circle = FALSE,
      marker = FALSE,
      edit = FALSE,
      polygon = TRUE,
      rectangle = TRUE,
      remove = TRUE,
      singleLayer = TRUE  # This allows only 1 polygon at a time when TRUE
    )
  
  observeEvent(input$map_selectbox_features, {
    myMap <- addGeoJSON(myMap, input$map_selectbox_features, color="green")
    # coord <- input$mymap_selectbox_features$features$geometry$coordinates
  })
  
  output$map <- renderLeaflet({myMap})
  return(input)
}


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