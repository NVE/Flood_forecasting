# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(plotly)
load("HBV_2014_GG.RData")

forecast_plot <- function(dat) {
  
  d <- ggplot(dat, aes(x= ymd(time_vec), y= values, group=variables, colour=variables ) ) +
       geom_line(size=2) +
       facet_grid(.~regine_main)
  ggplotly(d)
  invisible(d)
  
}


