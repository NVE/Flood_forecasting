library(reshape2)
library(zoo)
library(ggplot2)
library(plyr)
library(shiny)
library(hydroGOF)
library(topmodel)
library(DT)



load("./data/AllStationsAllModels1.RData")
source(file = "//nve/fil/h/Fastgrupper/Flomvarsling/FoU/Prosjekt/80103_Hydrologiske_prognoser/arbeidspakker/AP5/DDM/skript/script/hitrate.function.R")
#row.names(stnDatMaster) <- seq(nrow(stnDatMaster)) #removes strange row names from the data frame

#names(stnDatMaster)[11]<-"Catchment"

stationNames<-unique(stnDatMaster$stN)
modelNames<-unique(stnDatMaster$model)

listForecast<-list.files(path="./data/",pattern = "_AllStationsForecasts.RData")

load(paste("./data/", listForecast[1],sep = ""))
names(ddmFdataM)[5]<-strsplit(listForecast[1],"_")[[1]][1]
forecastDat<-ddmFdataM

for (i in 2:length(listForecast)){
  load(paste("./data/", listForecast[i],sep = ""))
  forecastDat<-cbind(forecastDat,ddmFdataM[,5])
  names(forecastDat)[i+4]<-strsplit(listForecast[i],"_")[[1]][1]
}
row.names(forecastDat) <- NULL 