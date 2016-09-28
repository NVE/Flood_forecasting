# setwd("./byman")
source("./global.R")


server <- function(input, output,session) {
  
  filename=reactive({
    paste0(input$torre,input$tipo)
  })
  day1=reactive({
    as.POSIXct(input$date1)
  })
  day2=reactive({
    as.POSIXct(input$date2)    
  })
  
  var <- reactive({
    input$catch
  })
  
  output$mytable1 = renderTable({
    allmeas
  })
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$allmeas,
           "76.5_Nigardsbrevatn"=Nigardsbrevatn,
           "55.4_Roykenes"=Roykenes,
           "2.604_Elverum"= Elverum,
           "2.32_Atna"=Atna,
           "2.11_Narsjo"= Narsjo)
  })
  output$mytable1 <- renderTable({
    allmeas
  })
  
  # choose columns to display
  # sorted columns are colored now because CSS are attached to them
  
  output$mytable2 = renderDataTable({
    mymeas <- subset(allmeas, Catchment %in% input$catch)
    mymeas <- DT::datatable(mymeas[ ,c("Model",input$show_meas)])
  }, options = list(orderClasses = TRUE))
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 = renderDataTable({
    allmeas
  }, options = list(orderClasses = TRUE))
  
  
  myText <- renderText({ " Leave this blank " })
  
  # input$date and others are Date objects. When outputting
  # text, we need to convert to character; otherwise it will
  # print an integer rather than a date.
  
  output$dateText <- renderText({
    paste("input$date is", as.character(input$dataRange))
  })
  
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  
  output$dateRangeText2 <- renderText({
    paste("input$dateRange2 is", 
          paste(as.character(input$dateRange2), collapse = " to ")
    )
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$cat)
  })
  
  callModule(mydygraphModule,"main_page") 
  callModule(mydygraph2Module,"main_page")

  callModule(TD_module,"main_page")

  
}