# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(plotly)
# load("HBV_2014_GG.RData")

# This function only produces "static" ggplot for the moment, but I need to find a way to do the shading with plotly
forecast_plot_shading <- function(dat) {

  dat$time <- as.Date(dat$time)
  #Shading for current day
  today <- Sys.Date()
  current_day <- data.frame(start = as.Date(today), end = as.Date(today + 1) )
#   current_day = read.table(textConnection("start, end
#       2016-07-11, 2016-07-12"), sep=',',
#       colClasses=c('Date', 'Date'), header=TRUE)

  d <- ggplot() +
      geom_line(data = dat, aes(x = time, y = Values, col = Variable), size = 1) +
    geom_rect(data=current_day, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='pink', alpha=0.2) +
      facet_grid(Type ~ ., scales="free_y") +
      theme_bw() + 
      scale_x_date(date_breaks = "1 day", date_labels = "%m %d")

  # p <- ggplotly(d) # %>% add_trace(x = c(ymd(2016-07-11), ymd(2016-07-12)), y = c(10, 10), fill = "tonexty")
  return(d)
  
}


forecast_plot <- function(dat) {
  
  dat$time <- as.Date(dat$time)
  d <- ggplot(dat, aes(x = time, y = Values))  +
    geom_line(aes(col = Variable), size = 1) +
    facet_grid(Type ~ ., scales="free_y") +
    theme_bw() +
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d")
  
  return(ggplotly(d))
  
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
