#multiplot function
#This is the definition of multiplot. It can take any number of plot objects as arguments, or if it can take a list of plot objects passed to plotlist.
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



stnForecast <- forecastDat[(212 + 3):(212 + 33),] # extract data for Fura
stnF1<-stnForecast[,1:3]
stnF0<-reshape(stnForecast,idvar = "dato",varying = list(names(stnForecast)[2:8]),times = names(stnForecast)[2:8],v.names="Value",direction ="long")
stnF0$Value<-as.numeric(stnF0$Value)

# pt<-ggplot(subset(stnF0,time %in% "temp"))+
#   theme(plot.background = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         #axis.text = element_blank(),  
#         axis.title = element_blank())+
#   geom_line(mapping=aes(x=dato, y=Value, group=1),colour="brown") + 
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  d1<-as.numeric(stnF0$dato[1]-0.5) #first date
  d2<-as.numeric(unique(stnF0$dato)) # for plotting purpose
  d0<-as.numeric(Sys.Date())     #todays date

prt<-ggplot(subset(stnF0,time %in%"nedb")) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank())+
  geom_bar( mapping=aes(x=dato, y=Value,group=1),fill="#56B4E9",colour="lightblue", stat="identity") +guides(fill=FALSE)+ 
  geom_line(subset(stnF0,time %in% "temp"),mapping=aes(x=dato, y=Value, group=1),colour="brown")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  geom_vline(xintercept = d1, colour = "wheat4", linetype=1, size=2)+
  geom_vline(xintercept = d0, colour = "#990000", linetype="longdash", size=1)+
  # geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=2)+
  # geom_hline(yintercept = -20, colour = "white", linetype=1) +
  # geom_hline(yintercept = -15, colour = "white", linetype=1) +
  # geom_hline(yintercept = -10, colour = "white", linetype=1) +
  # geom_hline(yintercept = -5, colour = "white", linetype=1) +
  # #geom_hline(yintercept = 0, colour = "white", linetype=1) +
  # geom_hline(yintercept = 5, colour = "white", linetype=1) +
  # geom_hline(yintercept = 10, colour = "white", linetype=1) +
  # geom_hline(yintercept = 15, colour = "white", linetype=1) +
  # #geom_hline(yintercept = 20, colour = "white", linetype=1) +
  #geom_hline(yintercept = 25, colour = "white", linetype=1) +
  # geom_hline(yintercept = 30, colour = "white", linetype=1) +
  # geom_hline(yintercept = 35, colour = "white", linetype=1) +
  # geom_hline(yintercept = 40, colour = "white", linetype=1)+
  # geom_hline(yintercept = 50, colour = "white", linetype=1) +
  # geom_hline(yintercept = 55, colour = "white", linetype=1) +
  # geom_hline(yintercept = 60, colour = "white", linetype=1) +
  # geom_hline(yintercept = 65, colour = "white", linetype=1) +
  # geom_hline(yintercept = 70, colour = "white", linetype=1) +
  # geom_hline(yintercept = 75, colour = "white", linetype=1) +
  # geom_hline(yintercept = 80, colour = "white", linetype=1)+
  # geom_hline(yintercept = 85, colour = "white", linetype=1) +
  # geom_hline(yintercept = 90, colour = "white", linetype=1) +
  # geom_hline(yintercept = 95, colour = "white", linetype=1) +
  # geom_hline(yintercept = 100, colour = "white", linetype=1)+

  geom_vline(xintercept = d2[2], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[4], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[6], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[8], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[10], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[12], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[14], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[16], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[18], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[20], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[22], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[24], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[26], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[28], colour = "wheat4", linetype=3, size=0.5) +
  ylab("Rainfall / Temp")


prr1<-ggplot(data=subset(stnF0,time %in% c("Obs","gbm","m5c","nnet","svm")), aes(x=dato, y=Value, group=time, shape=time,colour=time)) + 
  geom_line(aes(linetype=time), size=0.6) + scale_fill_manual(values=c("black","red", "purple", "blue","green")) +    # Set linetype by sex
      geom_point(size=1, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_colour_hue(name="Model",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Model",
                     values=c(20,21,22,23,24)) +      # Use points with a fill color
  scale_linetype_manual(values=c("solid","dashed", "dotted","dotdash","longdash"))+ # Change linetypes
  scale_linetype_discrete(name="Model") +
  
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  #ggtitle("Forecasts") +     # Set title
  #theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank())+
  theme(legend.position="bottom")+
  geom_vline(xintercept = d1, colour = "wheat4", linetype=1, size=2)+
  geom_vline(xintercept = d0, colour = "#990000", linetype="longdash", size=1)+
  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=2) +geom_vline(xintercept = d1, colour = "wheat4", linetype=1, size=2)+
  geom_vline(xintercept = d2[2], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[4], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[6], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[8], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[10], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[12], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[14], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[16], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[18], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[20], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[22], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[24], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[26], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[28], colour = "wheat4", linetype=3, size=0.5) +
  ylab("flow m^3/s")




prr<-ggplot(subset(stnF0,time %in% c("gbm","m5c","nnet","svm"))) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank())+
  geom_line( mapping=aes(x=dato, y=Value, group=time,linetype=time, colour=time))+ #geom_point(aes(x=dato, y=Value,group=time,fill="white"))+
  scale_fill_manual(values=c("red", "purple", "blue","green")) +theme(legend.position="bottom")+
  geom_point(mapping=aes(x=dato, y=Value, group=time,size=1))+#scale_fill_manual(values=c("red", "purple", "blue","green"))+
    geom_vline(xintercept = d1, colour = "wheat4", linetype=1, size=2)+
    geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=2) +geom_vline(xintercept = d1, colour = "wheat4", linetype=1, size=2)+
  #geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=2)+
  #geom_hline(yintercept = 5, colour = "white", linetype=1) +
  #geom_hline(yintercept = 10, colour = "white", linetype=1) +
  # geom_hline(yintercept = 15, colour = "white", linetype=1) +
  # geom_hline(yintercept = 20, colour = "white", linetype=1) +
  # geom_hline(yintercept = 25, colour = "white", linetype=1) +
  # geom_hline(yintercept = 30, colour = "white", linetype=1) +
  # geom_hline(yintercept = 35, colour = "white", linetype=1) +
  # geom_hline(yintercept = 40, colour = "white", linetype=1)+
  # geom_hline(yintercept = 50, colour = "white", linetype=1) +
  # geom_hline(yintercept = 55, colour = "white", linetype=1) +
  # geom_hline(yintercept = 60, colour = "white", linetype=1) +
  # geom_hline(yintercept = 65, colour = "white", linetype=1) +
  # geom_hline(yintercept = 70, colour = "white", linetype=1) +
  # geom_hline(yintercept = 75, colour = "white", linetype=1) +
  # geom_hline(yintercept = 80, colour = "white", linetype=1)+
  # geom_hline(yintercept = 85, colour = "white", linetype=1) +
  # geom_hline(yintercept = 90, colour = "white", linetype=1) +
  # geom_hline(yintercept = 95, colour = "white", linetype=1) +
  # geom_hline(yintercept = 100, colour = "white", linetype=1)+
  
  geom_vline(xintercept = d2[2], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[4], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[6], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[8], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[10], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[12], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[14], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[16], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[18], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[20], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[22], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[24], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[26], colour = "wheat4", linetype=3, size=0.5) +
  geom_vline(xintercept = d2[28], colour = "wheat4", linetype=3, size=0.5) +
  ylab("flow m^3/s")
  
  

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.25,0.75))))
print(prt, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(prr1, vp=viewport(layout.pos.row=2,layout.pos.col=1))

multiplot(prt, prr, cols=1)

