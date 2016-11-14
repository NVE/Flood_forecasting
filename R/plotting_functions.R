# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(plotly)


#' forecast_plot_shading
#' @description This function only produces "static" ggplot for the moment, but I need to find a way to do the shading with plotly
#' @param dat 
#' 
#' @return
#' @export
#'
#' @examples
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

#' forecast_plot
#'
#' @param dat 
#' @importFrom plotly ggplotly
#' @return
#' @export
#'
#' @examples
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

#' multimod_forecast_plot
#'
#' @param dat_1 
#' @param dat_2 
#' @param dat_3 
#' @param dat_4 
#' @param return_levels 
#' @param gg_plot 
#'
#' @return
#' @export
#'
#' @examples
multimod_forecast_plot <- function(dat_1 = NULL, dat_2 = NULL, dat_3 = NULL, dat_4 = NULL, return_levels = NULL, gg_plot = FALSE) {
  
  # The palette with grey:
  # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # The palette with black:
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  d <- ggplot() + scale_colour_manual(
    values = c("Obs" = cbPalette[1],"SimRaw" = cbPalette[2],"Sim.sim" = cbPalette[2],
               "SimCorr" = cbPalette[3],"Sim.sim.corr" = cbPalette[3], "Sim.obs" = cbPalette[4],
               "DDD.Sim" = cbPalette[5],"SimPrecipM50" = cbPalette[6], "SimPrecipP50" = cbPalette[6],
               "SimH50" = cbPalette[7], "SimL50" = cbPalette[7],
               "SimH90" = cbPalette[8], "SimL90" = cbPalette[8],
               "mean" = "yellow", "5Y" = "orange", "50Y" = "red"))
  p <- 0
  # We check every that each dataset is not an empty data frame.
  if (is.data.frame(dat_1) && nrow(dat_1) > 0) {
    dat_1$time <- as.Date(dat_1$time)
    d <- d + geom_line(data = dat_1, aes(x = time, y = Values, col = Variable), size = 1, linetype = 2)
    p <- 1
  }
  
  if (is.data.frame(dat_2) && nrow(dat_2) > 0) {
    dat_2$time <- as.Date(dat_2$time)
    d <- d + geom_line(data = dat_2, aes(x = time, y = Values, col = Variable), size = 1, linetype = 1)
    p <- 1
  }
  
  if (is.data.frame(dat_3) && nrow(dat_3) > 0) {
    dat_3$time <- as.Date(dat_3$time)
    d <- d + geom_line(data = dat_3, aes(x = time, y = Values, col = Variable), size = 1, linetype = 1)
    p <- 1
  }
  
  if (is.data.frame(dat_4) && nrow(dat_4) > 0) {
    dat_4$time <- as.Date(dat_4$time)
    d <- d + geom_line(data = dat_4, aes(x = time, y = Values, col = Variable), size = 1, linetype = 1)
    
#     d <- d + geom_line(data = subset(dat_4, Variable="Obs"), aes(x = time, y = Values), size = 1)
#     d <- d + geom_line(data = subset(dat_4, Variable="Sim.obs"), aes(x = time, y = Values), size = 1)
    p <- 1
  }
  
  if (is.data.frame(return_levels) && nrow(return_levels) > 0) {
    # return_levels$time <- as.Date(dat_1$time)
    print("return_levels")
        print(return_levels)
    d <- d + 
      geom_hline(data = return_levels, aes(yintercept = Values, col = Variable), size = 1, linetype = 5)
    p <- 1
  }

#   today <- Sys.Date()
#   today <- which(dat_1$time == today)
#   print("today")
#   print(today)
#   d <- d + geom_vline(xintercept = today, linetype="dashed", 
#                       color = "blue", size=1)
  if (p == 1) {
    d <- d +
      facet_grid(nbname ~ . , scales = "free") +
      theme_bw() + 
      scale_x_date(date_breaks = "1 day", date_labels = "%m %d") +
      theme(axis.title.x = element_blank()) +   # Remove x-axis label
      ylab("Runoff (m3/s)")                       # Set y-axis label
      # theme(axis.text.x = element_text(angle = 90, size = 12) ) # plot.margin = unit(c(10,0,0,0),"mm")
    # theme(plot.margin=unit(c(0,0,0,0),"mm")) 
      
  }

  # l <- plotly_build(d)  # %>%  layout(margin = list(l=100)) 
  # l$layout$margin$l <- l$layout$margin$l + 100
  if (gg_plot == TRUE) {
    return(d)
  } else {
    return(ggplotly(d))  
  }
  
  # return(l)
}

#' multimod_forecast_plot_EXP
#' @description To tidy up probable double up with multimod_forecast_plot
#' @param dat_1 
#' @param dat_2 
#' @param dat_3 
#' @param dat_4 
#'
#' @return
#' @export
#'
#' @examples
multimod_forecast_plot_EXP <- function(dat_1 = NULL, dat_2 = NULL, dat_3 = NULL, dat_4 = NULL) {
  
#   print("prout")
#   print(summary(dat_1))
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
  
  if (length(dat_4) > 0) {
    dat_4$time <- as.Date(dat_4$time)
    d <- d + geom_line(data = dat_4, aes(x = time, y = Values, col = Variable), size = 1, linetype = 4)
  }
  d <- d +
    theme_bw() + 
    scale_x_date(date_breaks = "1 day", date_labels = "%m %d") +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    ylab("Runoff (m3/s)")                       # Set y-axis label
    
  
  # if (length(dat_1) > 0 | length(dat_2) > 0 | length(dat_3) > 0 | length(dat_4) > 0) {
  d <- d +
    facet_grid(nbname ~ . , scales = "free")
  # }
  
  return(ggplotly(d))
  
}
