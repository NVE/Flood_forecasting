library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)
load("HBV_2014_GG.RData")



test2 <- dplyr::filter(HBV_2014_GG, regine_main %in% c("2.11", "2.28"))
d <- ggplot(test2, aes(x= ymd(time_vec), y= values, group=variables, colour=variables ) ) +     geom_line(size=2) + facet_grid(.~regine_main)
ggplotly(d)