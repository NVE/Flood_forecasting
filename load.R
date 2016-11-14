## Lena examples
#find fiel to read in on list (files_statfolder), and read in with both tab and whitespace delimiters: fill=FALSE was crucial here

# Q_name <- files_statfolder[which(grepl("QJ_", files_statfolder) == TRUE)]
# Q_file <- read.table(paste(paste("//nve/fil/h/HM/Eksterne Prosjekter/ExPrecFlood/WP_Aktiviteter/WP1_Trender og metoder_Dagensklima/SCHADEX/", 
#                                  mynames_norsk[i], "/", Q_name , sep="") ), skip = 4, sep = "\t", fill = F)
# tmp <- strsplit(as.character(Q_file$V1), "00:00")
# Q_file <- data.frame(matrix(unlist(tmp), nrow=nrow(Q_file), byrow=T))

##################################################################################################
library(NVEDATA)

metadata <- get_metadata()


## Reading DDD model results
# Operational path is /hdata/drift/flom/DDD24h2015R/24hres.txt

file_connect <- file("./data/DDD24h2015R/24hres.txt", open = "rt")

# x <- TRUE
# while (x) {
# x <- !grepl(":", readLines(file_connect, n = 1))
# }

DDD <- read.table(file_connect, sep = "\t", fill = TRUE)
DDD2 <- head(test, 30L)  # To keep the data from only 1 station  
close(file_connect)

## Reading HBV model +50% precipitation results
# Operational path is /hdata/drift/flom/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt
file_connect <- file("./data/usikkerhet_grd/ut_test/vfpost_usikkerhet.txt", open = "rt")

# x <- TRUE
# while (x) {
#   x <- !grepl("Dato", readLines(file_connect, n = 1))
# }

HBV50 <- read.table(file_connect, sep = "", fill = TRUE)
HBV50_2 <- head(test, 21L)  # To get the past data of 1 station only
HBV50_3 <- tail(test, 9L)  # To get the forecast data of 1 station only (9 day forecast)

close(file_connect)

## Reading HBV model (normal run) results. The file structure is the same as the +50% results
# Operational path is /hdata/drift/flom/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt
file_connect <- file("./data/usikkerhet_grd/utskrift/vfpost_usikkerhet.txt", open = "rt")

HBV <- read.table(file_connect, sep = "", fill = TRUE)
HBV_2 <- head(test, 21L)  # To get the past data of 1 station only
HBV_3 <- tail(test, 9L)  # To get the forecast data of 1 station only (9 day forecast)

close(file_connect)


## Reading ODM model results. There is a folder per station.
# Operational path is (for 1st station only):
# /hdata/drift/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt
file_connect <- file("./data/flood/H-VEPS02/simu_hbv_elev_24h/AAMOT_ELEV_24h/InputTimeSeries.txt", open = "rt")
readLines(file_connect, n = 6)
# DATE / TEMP / PRECIP / TEMP / PRECIP / PRECIP / TEMP / DISCHARGE
ODM <- read.table(file_connect, sep = "", fill = TRUE)
colnames(ODM) <-  c("DATE", "TEMP", "PRECIP", "TEMP", "PRECIP", "PRECIP", "TEMP", "DISCHARGE")

