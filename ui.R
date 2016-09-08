# This is the user-interface definition of a Shiny web application.

ui <- navbarPage("Flomvarsling", collapsible = TRUE, theme = "my_style.css",
                 
#                  tabPanel("About", icon = icon("info"),
#                           
#                           fluidRow(
#                             column(6, wellPanel(
#                               wellPanel(includeMarkdown("about.md")))
# 
#                               ),
#                             column(6,
#                               img(src='flood.jpg', align = "right")
#                             )),
#                           # TAB 1
#                           fluidRow(
#                             column(6,
#                                    wellPanel(includeMarkdown("tab1.md"))
#                                    ),
#                             column(6,
#                                    img(id = "tab1", src='multimodel_tab_with_warning.png', align = "right")
#                           )),
#                           bsModal("modalExample", "Your plot", "tab1", size = "large",
#                                   img(src='multimodel_tab_with_warning.png', align = "center")),
#                           bsTooltip("tab1", "Click to enlarge", 
#                                     "left", options = list(container="body")),
#                  
#                           # TAB 2
#                           fluidRow(
#                             column(6,
#                                    wellPanel(includeMarkdown("tab2.md"))),
#                             column(6,
#                                    img(id = "tab2", src='multimodel_tab_with_warning.png', align = "right")
#                             )),
#                           bsModal("modalExample", "Your plot", "tab2", size = "large",
#                                   img(src='multimodel_tab_with_warning.png', align = "center")),
#                           # TAB 3
#                           fluidRow(
#                             column(6,
#                                    wellPanel(includeMarkdown("tab3.md"))),
#                             column(6,
#                                    img(id = "tab3", src='multimodel_tab_with_warning.png', align = "right")
#                             )),
#                           bsModal("modalExample", "Your plot", "tab3", size = "large",
#                                   img(src='multimodel_tab_with_warning.png', align = "center"))
#           
#                  ),

                 navbarMenu("Multi-station / Multi-model", icon = icon("random"),
                  tabPanel("DropDown station selection", 
                          mapModuleUI("multistation_map", multiple = TRUE),
                          multimod_forecast_selection_modUI("multistation_plot"),
                          forecast_plot_modUI("multistation_plot")
                  ),
                  tabPanel("Polygon selection",
                           ## Commented: first intended way to do the multi-station multi-model tab
         #                                      mapModule_polygonFeatureUI("map_polygon"),
         #                                      multimod_forecast_selection_modUI("multi_plot")
         # forecast_plot_modUI("multi_plot")
                          NEW_mapModule_polygonFeatureUI("map_polygon")
                  )),
                 navbarMenu("Forecast - single model", icon = icon("line-chart"),
                            tabPanel("HBV_2014",
                                     mapModuleUI("map_HBV_2014"),
                                     OLD_forecast_plot_modUI("forecast_plot_HBV_2014")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2014")
                                     ),
                            tabPanel("HBV_2016",
                                     mapModuleUI("map_HBV_2016"),
                                     OLD_forecast_plot_modUI("forecast_plot_HBV_2016")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_HBV_2016")
                            ),
                            tabPanel("DDD",
                                     mapModuleUI("map_DDD"),
                                     OLD_forecast_plot_modUI("forecast_plot_DDD")
                                     # forecast_plot_mod_shadingUI("forecast_plot_shading_DDD")
                            )
                 ),
                 navbarMenu("Table tools", icon = icon("globe"),
                            tabPanel("Metadata",
                                     table_modUI("metadata_table")
                            ),
                            tabPanel("Return levels",
                                     table_modUI("RL_table")
                            ),
                            tabPanel("HBV_2014",
                                     table_modUI("HBV_2014_table")
                            ),
                            tabPanel("HBV_2016",
                                     table_modUI("HBV_2016_table")
                            ),
                            tabPanel("DDD",
                                     table_modUI("DDD_table")
                            )
                 ),
                navbarMenu("Historical tools", icon = icon("history"),
                            tabPanel("Past forecasting performance"
#                                      mapModuleUI("past_map"),
#                                      multimod_forecast_selection_modUI("past_plot"),
#                                      multimod_forecast_plot_modUI("past_plot")
                                    ),
                             tabPanel("Past events"),
                             tabPanel("Calibration results")
)
)
      

# # From Radiant app. To integrate
# help_and_report(modal_title = "Design of Experiments", fun_name = "doe",
#                 help_file = inclMD(file.path(r_path,"analytics/tools/help/doe.md")))

# output$dtree_save_yaml <- downloadHandler(
#   filename = function() {"dtree.yaml"},
#   content = function(file) {
#     isolate({
#       cat(paste0(input$dtree_edit,"\n"), file = file)
#     })
#   }
# )
