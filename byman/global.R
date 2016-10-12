library(reshape2)
library(ggplot2)
library(plyr)
library(shiny)
library(hydroGOF)
library(topmodel)
library(DT)
library(dygraphs)
library(zoo)
library(plotrix)
library(dplyr)
library(tidyr)

# Visualizatin development



dataDir <- "//nve/fil/h/Fastgrupper/Flomvarsling/FoU/Prosjekt/80103_Hydrologiske_prognoser/arbeidspakker/AP5/DDM/projects/Flood_forecasting/data"

load(file.path(dataDir,"AllStationsAllModels0.RData"))
#source(file = "//nve/fil/h/Fastgrupper/Flomvarsling/FoU/Prosjekt/80103_Hydrologiske_prognoser/arbeidspakker/AP5/DDM/skript/script/hitrate.function.R")

source(file = file.path(dataDir,"hitrate_alt2.function.R"))

#row.names(stnDatMaster) <- seq(nrow(stnDatMaster)) #removes strange row names from the data frame

#names(stnDatMaster)[11]<-"Catchment"

stationNames <- unique(stnDatMaster$stN)
modelNames <- unique(stnDatMaster$model)

listForecast <- list.files(path=dataDir,pattern = "_AllStationsForecasts.RData")

load(file.path(dataDir, listForecast[1]))
names(ddmFdataM)[5 ] <- strsplit(listForecast[1],"_")[[1]][1]
forecastDat <- ddmFdataM

for (i in 2:length(listForecast)){
  load(file.path(dataDir, listForecast[i]))
  forecastDat <- cbind(forecastDat,ddmFdataM[,5])
  names(forecastDat)[i+4] <- strsplit(listForecast[i],"_")[[1]][1]
}
row.names(forecastDat) <- NULL 