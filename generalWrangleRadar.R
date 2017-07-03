library(ncdf4)
library(data.table)
library(sp)
library(raster)
library(proj4)


#De Bilt 140844, 457994

pointX<-140844
pointY<-457994

  
convertToPolar<-function(pointX, pointY){  

RDH<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs"

pointDF<-data.frame(pointX,pointY)

coordinates(pointDF)<-~pointX+pointY
crs(pointDF)<-RDH

polar<-"+proj=stere +lat_0=90 +lon_0=0.0 +lat_ts=60.0 +a=6378.137 +b=6356.752 +x_0=0 +y_0=0"

polars<-ptransform(cbind(pointDF$pointX,pointDF$pointY),RDH,polar, silent = F)

polars<-data.frame(polars)
colnames(polars)<-c("X","Y","Z")

#polar is a dataFrame
polars

}



findCells<-function(filepath, dfCoordinatesPolar, variableOfInterest = "image1_image_data"){


#getting the coordinates from the polar dataframe
xvar <- dfCoordinatesPolar$X
yvar <- dfCoordinatesPolar$Y
#loading NetCDF file of wind
fileFull<-nc_open(filepath)
  
#reading in the variables
x<-ncvar_get(fileFull, "x")
y<-ncvar_get(fileFull, "y")
dataGrid<-ncvar_get(fileFull,variableOfInterest)

  
    
  
cellSize<-x[[2]]-x[[1]] #constant size square grid is assumed in this projection
offsetGrid<-cellSize/2 #x,y coordinate of the grid are mid points


xmin<-min(x)
ymin<-min(y)

xcell<-ceiling((xvar-xmin+offsetGrid)/cellSize)#getting to the NetCDF array cell from the coordinate
ycell<-ceiling((yvar-ymin+offsetGrid)/cellSize)#

list(x=xcell, y=ycell)

}




getMetaDataNetCDF<-function(filepath, variableOfInterest = "image1_image_data"){
  missingValue <- ncatt_get(fileFull,variableOfInterest,"_FillValue")
  missingValue <- missingValue$value
  scaleFactor <- ncatt_get(fileFull,variableOfInterest,"scale_factor")
  scaleFactor <- scaleFactor$value
  addOffsetFactor <- ncatt_get(fileFull,variableOfInterest,"add_offset")
  addOffsetFactor <- addOffsetFactor$value
  
  list(missingValue = missingValue, scaleFactor = scaleFactor, addOffset = addOffsetFactor, variable = variableOfInterest)
}

