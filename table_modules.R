library(DT)

table_modUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(DT::dataTableOutput(ns("table")))
}


table_mod <- function(input, output, session, dat) {

output$table <- DT::renderDataTable({
  datatable(dat,
            # extensions = 'Scroller', 
            filter = 'top', 
            options = list(
              # dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), # this was for extensions = 'Buttons' which didn't work
              
              #               deferRender = TRUE,
              #               scrollY = 200,
              #               scroller = TRUE,  
              
              pageLength = 20, autoWidth = TRUE, 
              
              order = list(list(2, 'asc'))
            )
  )  
  #     %>% formatStyle(
  #     'Station.name',
  #      backgroundColor = styleInterval(3.4, c('white', 'grey'))
  #    )  # Can be used for additional styling
})

}