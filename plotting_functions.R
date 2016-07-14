# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(plotly)
load("HBV_2014_GG.RData")

forecast_plot <- function(dat) {
  
  d <- ggplot(dat, aes(x = ymd(time), y = values))  +
    geom_line(aes(col = Variable), size = 1) +
    facet_grid(Type ~ ., scales="free_y") +
    theme_bw()
  ggplotly(d)
  invisible(d)
  
}


forecast_plot_simple <- function(dat) {
  
  d <- ggplot(dat, aes(x = ymd(time_vec), y = values, group = variables, colour = variables ) ) +
    geom_line(size = 1.5) +
    facet_grid(.~regine_main) +
    theme_bw()
  ggplotly(d)
  invisible(d)
  
}

single_station_map <- function(stations, selected_regine_main,
                                         selected_name,
                                         selected_long,
                                         selected_lat) {

  map <- leaflet() %>% addTiles() %>%
  setView(13, 64, zoom = 5)  %>%
  addCircleMarkers(data = stations, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(stations$name), "Number:", stations$regine_main,
                                 sep = " "), radius = 5, 
                   color = "black",  #  ~my.color.func(station$length_rec, my.colors), 
                   stroke = FALSE, fillOpacity = 0.5,
                   layerId = stations$regine_main) %>%
  addPopups(selected_long, selected_lat, paste("Name:", as.character(selected_name), "Number:", 
                                                               selected_regine_main, sep = " "),
            options = popupOptions(closeButton = FALSE, maxWidth = 100)) 

#  %>%

#     addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
#               title = "Length of flood record (years)",
#               opacity = 1)
  return(map)
}
