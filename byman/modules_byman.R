mydygraphModule <- function(input, output, session) {
  
#   alldat$myDate <- as.Date(alldat$myDate)
#   dat1<-subset(alldat, Catchment %in% input$catch)
#   dat1<-dat1[,-2]
#   dat2<-dat1[complete.cases(dat1),]
#   dat.z<-zoo(dat2[,2:8],dat2$myDate)
#   myts<-as.ts(dat.z)
  
 
#       alldat$myDate <- as.Date(alldat$myDate)
#       dat1<-subset(alldat, Catchment %in% input$catchment)
#       dat1<-dat1[,-2]
#       dat2<-dat1[complete.cases(dat1),]
#       dat.z<-zoo(dat2[,2:8],dat2$myDate)
#       myts<-as.ts(dat.z)
# 
#     
#     print("in module function")
#     print(head(myts))
# 
#   
#   output$module_graph <- renderDygraph({
#     myts
#       #%>%dyRangeSelector()
#    
#     
#     # myts#%>%dyRangeSelector()
#   })
#   
  ##########################
 output$module_graph <- renderDygraph({
    
    alldat$myDate <- as.Date(alldat$myDate)
    
    dat_module<-subset(alldat, Catchment %in% input$catchment)

   
        dat_cropped<-dat_module[,-2]
    dat2<-dat_cropped[complete.cases(dat_cropped),]
    dat.z1<-zoo(dat2[,2:8],dat2$myDate)

    myts<-as.ts(dat.z1)

    dygraph(
      myts#%>%dyRangeSelector()
    )
  })
  
  
  
  
  
  
  
  
  
}


mydygraphModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  
  fluidRow(
    selectInput(ns("catchment"), 'Catchment to analyse', as.character(unique(alldat$Catchment))),
  dygraphOutput(ns("module_graph"),height = 600)
  )
  
}




######################################################

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


# MAP OF NORWAY WITH COLOR CHANGING ACCORDING TO THE NUMBER OF DATA.
norway_map4server <- function(selected.station) {
  
  st.index <- which(station$number == selected.station)
  st.name <- station$name[st.index]
  st.long <- station$long[st.index]
  st.lat <- station$lat[st.index]
  st.length_rec <- station$length_rec[st.index]
  
  #   pal <- colorNumeric(
  #     palette = heat.colors(5),
  #     domain = c(0,30,60,90,120,150))
  # qpal <- colorQuantile("RdYlBu", length.bins, n = 5)
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,30,60,90,120,150)
    color <- my.colors[trunc(x2plot/30)+1]
    invisible(color)
  }
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$number,
                                 "Length of record:", station$length_rec, sep = " "), radius = 5, 
                   color = ~my.color.func(station$length_rec, my.colors), stroke = FALSE, fillOpacity = 0.5,
                   layerId = station$number) %>%
    
    addPopups(st.long, st.lat, paste("Name:", as.character(st.name), "Number:", selected.station,
                                     "Length of record:", st.length_rec, sep = " "),
              options = popupOptions(closeButton = FALSE, maxWidth = 100)) %>%
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}