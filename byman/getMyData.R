library(reshape2)
library(zoo)
library(ggplot2)
library(plyr)

library(shiny)
# library(ggplot2)
# library(zoo)
library(DT)

# setwd("F:\\ML\\VSRT\\newTest")


alldat <- read.table("./byman/data/allstationsallmodels.csv", header = TRUE, skip = 0, as.is=TRUE,sep = ",")
allmeas <- read.table("./byman/data/allmodelsperformancemeasures.csv", header = TRUE, skip = 0, sep = ",")
alldat$myDate <- as.Date(alldat$myDate)

mydat<-reshape(alldat,
               varying = c("Observed", "HBV", "NNET", "SVM", "GBM", "M5", "M5C"),
               v.names = "flow",
               timevar = "model", 
               times = c("Observed", "HBV", "NNET", "SVM", "GBM", "M5", "M5C"),
               new.row.names = 1:(dim(alldat)[1] * dim(alldat)[2]) ,
               direction = "long")
mydat<-mydat[,-5]

# ggplot(data = mydat, aes(x = myDate, y = flow)) + 
#   geom_line(aes(color = model), size = 1.25)
# 
# #Plotiing data series in ggplot
# p<-ggplot(data=subset(mydat, Catchment %in% "2.32_Atna" & model %in% "Observed")) +
#   geom_line( aes(x = myDate, y = flow), size = 0.7) +
#   geom_line(data=subset(mydat, Catchment %in% "2.32_Atna"), aes(x = myDate, y = flow, group = model, col = model), size = 0.7)
# 
# 
# 
# p1<-ggplot(subset(mydat, Catchment %in% "2.32_Atna" )) +
#   geom_line(aes(x = myDate, y = flow, group = model, col = model),  size = 0.7)
# 
# 
# 
# 
# p <- ggplot(subset(mydat, Catchment %in% "2.32_Atna")), aes(x = myDate, y = flow, group = model)) + geom_line(aes(colour =model)) +
#   geom_line(stat = "identity", subset = .(model == "Observed"), cost = "grey30") +
#   geom_bar(stat = "identity", subset = .(data == "mod"), fill = "grey50") +
#   stat_summary(fun.data = "median_hilow", geom = "crossbar", colour = data) +
#   geom_smooth(stat = "identity", aes(group = 2), subset = .(data == "mod"), colour = "red", linetype = 2)
# 
