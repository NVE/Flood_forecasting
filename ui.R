# This is the user-interface definition of a Shiny web application.

ui <- navbarPage("Flomvarsling",  # cut off:  id = "nav",
                 
                 tabPanel("About", icon = icon("info"),
                          
                          fluidRow(
                            column(8, wellPanel(
                              HTML('
<p style="margin-left:1em; style="text-align:justify"> Blabla... flood data in Norway </p>')
                              )    
                            ))
                          
                 ),
                 tabPanel("Forecast HBV_2014", icon = icon("line-chart"),
                          mapModuleUI("map1"),
                          forecast_plot_modUI("forecast_plot"),
                          forecast_plot_mod_shadingUI("forecast_plot_shading")

                 ),
                 navbarMenu("Mapping tools", icon = icon("globe"),
                            tabPanel("Map1"),
                            tabPanel("Map2")
                 )
)
      


    