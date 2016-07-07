

library(leaflet)
library(shiny)

# Modules for the Flood_forecasting app

# Module UI function

mapModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # fluidRow(
 leafletOutput(ns("map"))
 
 
  # )
  
}


mapModule2 <- function(input, output, session) {

#   selection <- reactive(input$mymap_selectbox_features)
  
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
      remove = TRUE
      # singleLayer = TRUE
    ) 
#   %>%
#     addGeoJSON(selection(), color="green")
#     
  output$map <- renderLeaflet({myMap})
  
}







##############################################

# EXAMPLES


# Module server function
map_server <- function(input, output, session) {
  
  
 
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
               remove = TRUE
               # singleLayer = TRUE
             ) %>%
           addGeoJSON(input$mymap_selectbox_features, color="green")
  
  
  observeEvent(input$mymap_selectbox_features, {
    output$mymap <- renderLeaflet({
      if( "radius" %in% names(input$mymap_selectbox_features$properties))
      {
        print("prout1")
        lng = input$mymap_selectbox_features$geometry$coordinates[[1]]
        lat = input$mymap_selectbox_features$geometry$coordinates[[2]]
        
        radius = input$mymap_selectbox_features$properties$radius
        myMap() %>% addCircles(lng = lng, lat = lat, radius = radius, color="green")
      }
      else{

        temp <- as.matrix(input$mymap_selectbox_features$features[[1]]$geometry$coordinates[[1]])
        print(length(temp[ , 1]))
        
        nb_points <- length(temp[ , 1]) - 1
        coord <- matrix(data = NA, nrow = nb_points, ncol = 2)
        for (i in 1:nb_points) {
          coord[i, ] <- c(input$mymap_selectbox_features$features[[1]]$geometry$coordinates[[1]][[i]][[1]],
                          input$mymap_selectbox_features$features[[1]]$geometry$coordinates[[1]][[i]][[2]])
        }
        print(coord)
        test_point <- coord[1, ]
        test_point2 <- coord[3, ]
        test_point <- (test_point + test_point2) / 2
        
        print("first point of poly")
        
        print(test_point)
        print("is point in poly")
        test <- point.in.polygon(test_point[1], test_point[2], coord[ , 1], coord[ , 2], mode.checked=FALSE)
        # integer array; values are: 
        # 0: point is strictly exterior to pol; 
        # 1: point is strictly interior to pol; 
        # 2: point lies on the relative interior of an edge of pol; 
        # 3: point is a vertex of pol.
        
        print(test)
        
        
        
        myMap() %>% addGeoJSON(input$mymap_selectbox_features, color="green")
        # coord <- input$mymap_selectbox_features$features$geometry$coordinates
        
      }
    })
  })
  
  observeEvent(input$mymap_selectbox_features, {

    output$summary <- renderPrint({
      #             print(str(input$mymap_selectbox_features$features$geometry))
      print("blab")
    })      
    
    
    
  })
  
  
}






## plotly filling exmaple

library(plotly)
p <- plot_ly(x = c(1, 2, 3, 4), y = c(0, 2, 3, 5), fill = "tozeroy")
add_trace(p, x = c(1, 2, 3, 4), y = c(3, 5, 1, 7), fill = "tonexty")

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}


# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}
