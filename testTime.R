test1<-as.POSIXct("2015-03-29 01:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")
test2<-as.POSIXct("2015-03-29 02:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")
test11<-as.POSIXct("2015-03-29 01:59:00", format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")
test3<-as.POSIXct("2015-03-29 03:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Amsterdam")


library(sp)
library(raster)
RDHx<-c(231125,182741.332)
RDHy<-c(581614, 579385.37)
dataFrameRDH<-data.frame(RDHx,RDHy)
coordsRDH<-CRS("+init=epsg:28992")
coordinates(dataFrameRDH)<-~RDHx+RDHy
crs(dataFrameRDH)<-coordsRDH
WGS84<<-CRS("+init=epsg:4326")
latLonDF<-spTransform(dataFrameRDH,WGS84)
latlonDF<-data.frame(latLonDF)





library(ncdf4)
library(data.table)
#fileH<-nc_open("data/INTER_OPER_R___FG1_____L3__20150322T000000_20150322T001000_0001.nc")

fileFull<-nc_open("data/RAD_NL25_RAC_MFBS_5min_201503281830.nc")

#lat<-ncvar_get(fileFull,"lat")
#lon<-ncvar_get(fileFull,"lon")
x<-ncvar_get(fileFull, "x")
y<-ncvar_get(fileFull, "y")
test<-ncvar_get(fileFull,"grid")
time<-ncvar_get(fileFull,"time")

coordPolar<-CRS("+init=epsg:9810")
xx<-data.frame(x,y)
coordinates(xx)<-~x



