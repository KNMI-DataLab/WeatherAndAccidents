library(ncdf4)
fileH<-nc_open("data/INTER_OPER_R___FG1_____L3__20150322T000000_20150322T001000_0001.nc")

lat<-ncvar_get(fileH,"lat")
lon<-ncvar_get(fileH,"lon")
x<-ncvar_get(fileH, "x")
y<-ncvar_get(fileH, "y")
test<-ncvar_get(fileH,"grid")


dataAccidents<-read.csv("data/accidents/ExportOngevalsData.csv")



##wrangling spatially the accidents and the wind speed
xvar<-dataAccidents[1:100,]$X
yvar<-dataAccidents[1:100,]$Y

xmin<-min(x)
ymin<-min(y)

cellSize<-x[[2]]-x[[1]] #constant size square grid is assumed in this projection
offsetGrid<-cellSize/2

xcell<-ceiling((xvar-xmin+offsetGrid)/cellSize)
ycell<-ceiling((yvar-ymin+offsetGrid)/cellSize)
wind<-test[cbind(xcell,ycell)]

dataAccidentsWind<-cbind(dataAccidents[1:100,],wind)
