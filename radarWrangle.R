


library(ncdf4)
library(data.table)
library(sp)
library(raster)
library(proj4)

urlPartial<-"http://opendap.knmi.nl/knmi/thredds/dodsC/radarprecipclim/RAD_NL25_RAC_MFBS_5min_NC/2015/03/RAD_NL25_RAC_MFBS_5min_"
#201503312255.nc

load("data/accidents/weekdataFiets.Rdata")
dataAccidentsBike<-weekdataFiets

dataAccidentsBike$datetime<-paste0(dataAccidentsBike$datum, ' ', dataAccidentsBike$Uur,":",dataAccidentsBike$minuut,":00")
dataAccidentsBike$datetime<-as.POSIXct(dataAccidentsBike$datetime, tz="Europe/Amsterdam", format = "%Y-%m-%d %H:%M:%S")

dataAccidentsBike<-data.table(dataAccidentsBike)

dataAccidentsBike$rounded5Min<-as.POSIXct(round(as.double(dataAccidentsBike$datetime)/(5*60))*(5*60), format = "%Y%m%d%H%M", origin = (as.POSIXlt("1970-01-01")))
dataAccidentsBike$suffixURL<-strftime(dataAccidentsBike$rounded5Min, tz="UTC", format = "%Y%m%d%H%M")
uniqueSuff<-unique(dataAccidentsBike$suffixURL)

urls<-paste0(urlPartial,uniqueSuff,".nc")

con<-lapply(urls, nc_open)

#####TO BE CONTINUED THE DATA ACCESS NETCDF OPENDAP####

RDH<-CRS("+init=epsg:28992")

coordinates(dataAccidentsBike)<-~X+Y
crs(dataAccidentsBike)<-RDH

polar<-"+proj=stere +lat_0=90 +lon_0=0.0 +lat_ts=60.0 +a=6378.137 +b=6356.752 +x_0=0 +y_0=0"

polars<-ptransform(cbind(dataAccidentsBike$X,dataAccidentsBike$Y),RDH,polar, silent = F)
cbind(dataAccidentsBike,polars)
#change the name of the variables



