
mydygraphModule <- function(input, output, session) {
  
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
