# Installing and loading required packages
packages <- c("shiny", "reshape2", "zoo", "plyr", "DT", "plotrix", "plyr", "dygraphs")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(shiny)
library(reshape2)
library(zoo)
library(DT)
library(plotrix)
library(plyr)
library(dygraphs)
library(stats)


source("./modules_byman.R")


alldat <- read.table("./data/allstationsallmodels.csv", header = TRUE, skip = 0, as.is = TRUE,sep = ",")
allmeas <- read.table("./data/allmodelsperformancemeasures.csv", header = TRUE, skip = 0, sep = ",")
alldat$myDate <- as.Date(alldat$myDate)

mydat <- reshape(alldat,
               varying = c("Observed", "HBV", "NNET", "SVM", "GBM", "M5", "M5C"),
               v.names = "flow",
               timevar = "model", 
               times = c("Observed", "HBV", "NNET", "SVM", "GBM", "M5", "M5C"),
               new.row.names = 1:(dim(alldat)[1] * dim(alldat)[2]) ,
               direction = "long")
mydat <- mydat[ , -5]
