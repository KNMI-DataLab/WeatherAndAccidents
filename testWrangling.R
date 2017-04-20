library(ncdf4)
fileH<-nc_open("data/INTER_OPER_R___FG1_____L3__20150322T000000_20150322T001000_0001.nc")

fileFull<-nc_open("/nobackup/users/pagani/weatherAccidents/03/output.nc")

lat<-ncvar_get(fileH,"lat")
lon<-ncvar_get(fileH,"lon")
x<-ncvar_get(fileH, "x")
y<-ncvar_get(fileH, "y")
test<-ncvar_get(fileH,"grid")
timeee<-ncvar_get(fileFull,"time")


dataAccidents<-read.csv("data/accidents/ExportOngevalsData.csv")
dataAccidents$date<-as.Date(dataAccidents$datum, "%d%b%y")
levels(dataAccidents$Uur)[levels(dataAccidents$Uur)=='Onbekend'] <- NA
dataAccidents$hour<-sapply(dataAccidents$Uur, function(y) strsplit(as.character(y),"\\.")[[1]][1])


load("data/accidents/weekdataFiets.Rdata")
dataAccidentsBike<-weekdataFiets

dataAccidentsBike$datetime<-paste0(dataAccidentsBike$datum, ' ', dataAccidentsBike$Uur,":",dataAccidentsBike$minuut,":00")
dataAccidentsBike$datetime<-as.POSIXct(dataAccidentsBike$datetime, tz="Europe/Amsterdam", format = "%Y-%m-%d %H:%M:%S")



##wrangling spatially the accidents and the wind speed
xvar<-dataAccidentsBike$X
yvar<-dataAccidentsBike$Y

xmin<-min(x)
ymin<-min(y)

cellSize<-x[[2]]-x[[1]] #constant size square grid is assumed in this projection
offsetGrid<-cellSize/2 #x,y coordinate of the grid are mid points

xcell<-ceiling((xvar-xmin+offsetGrid)/cellSize)
ycell<-ceiling((yvar-ymin+offsetGrid)/cellSize)
wind<-test[cbind(xcell,ycell)]

dataAccidentsWind<-cbind(dataAccidentsBike,wind)



# dataFolderWind<-"/nobackup/users/pagani/gsie/output/pagani/wind10min/Fg_1_oper_v0001/2015/"
# 
# nc.files<-list.files(dataFolderWind,pattern="*.nc",full.names=T, recursive = T)
# lapply()
# st2<-stack(nc.files,varname=c("time"))

