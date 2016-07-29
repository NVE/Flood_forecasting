# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(plotly)

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


forecast_plot_shading2 <- function(dat) {
  
  dat$time <- as.Date(dat$time)
  #Shading for current day
  today <- Sys.Date()
  current_day <- data.frame(start = as.Date(today), end = as.Date(today + 1) )
  #   current_day = read.table(textConnection("start, end
  #       2016-07-11, 2016-07-12"), sep=',',
  #       colClasses=c('Date', 'Date'), header=TRUE)
  
  d <- ggplot() +
    geom_line(data = dat, aes(x = time, y = Values, col = Variable), size = 1) +
#     geom_rect(data=current_day, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill='pink', alpha=0.2) +
    facet_grid(regine.main ~ . , scales = "free_y") +
    theme_bw() + 
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d")
  
  # p <- ggplotly(d) # %>% add_trace(x = c(ymd(2016-07-11), ymd(2016-07-12)), y = c(10, 10), fill = "tonexty")
  return(d)
  
}

forecast_plot <- function(dat) {
  
  dat$time <- as.Date(dat$time)
  
  d <- ggplot() +
    geom_line(data = subset(dat, Variable!="Precip"), aes(x = time, y = Values, col = Variable), size = 1) +
    geom_bar(data = subset(dat, Variable=="Precip"), aes(x = time, y = Values, col = Variable), size = 1, stat="identity", width = 0.4) + 
    facet_grid(Type ~ ., scales = "free_y") +
    theme_bw() +
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d")
  
  return(ggplotly(d))
  
}


multimod_forecast_plot <- function(dat_1, dat_2, dat_3) {
  
  d <- ggplot()
  
  if (length(dat_1) > 0) {
    dat_1$time <- as.Date(dat_1$time)
    d <- d + geom_line(data = dat_1, aes(x = time, y = Values, col = Variable), size = 1, linetype = 1)
  }
  
  if (length(dat_2) > 0) {
    dat_2$time <- as.Date(dat_2$time)
    d <- d + geom_line(data = dat_2, aes(x = time, y = Values, col = Variable), size = 1, linetype = 2)
  }
  
  if (length(dat_3) > 0) {
    dat_3$time <- as.Date(dat_3$time)
    d <- d + geom_line(data = dat_3, aes(x = time, y = Values, col = Variable), size = 1, linetype = 3)
  }
  
  d <- d +
    # facet_grid(Variable ~ ., scales = "free_y") +
    theme_bw() + 
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d")
  
  return(ggplotly(d))
  
}




forecast_plot2 <- function(dat) {
  
  dat$time <- as.Date(dat$time)
  d <- ggplot(dat, aes(x = time, y = Values))  +
    geom_line(aes(col = Variable), size = 1) +
    facet_grid(regine.main ~ . , scales = "free") +
    theme_bw() +
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d")
  
  return(ggplotly(d))
  
}
