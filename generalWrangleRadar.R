library(ncdf4)
library(data.table)
#library(sp)
#library(raster)
library(proj4)

# SOURCE contains the projection string of the external data
# this is currently the Dutch grid. 
# SOURCE should eventually be a parameter, if we want international use
# For now, we keep it as a global parameter

SOURCE <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs"


# projection string in the polar stereographic projection
# see alsoe http://adaguc.knmi.nl/contents/datasets/productdescriptions/W_ADAGUC_Product_description_RADNL_OPER_R___25PCPRR_L3.html
TARGET <- "+proj=stere +lat_0=90 +lon_0=0.0 +lat_ts=60.0 +a=6378.137 +b=6356.752 +x_0=0 +y_0=0"


# for development, we use a typical NetCDF file, containing radar data

exampleNetCDFfile <- "./KNMI/PrecipitationRadar/KNMI-Data/RAD_NL25_RAC_MFBS_5min_201501312355.nc"
filepath <- exampleNetCDFfile


# the location of the KNMI in the Dutch grid
#De Bilt 140844, 457994

pointX<-140844
pointY<-457994



# the convertToPolar function converts coordinates in the SOURCE projection to 
# coordintates in the TARGET projection

convertToPolar <- function(pointX, pointY) {  
  
  
  
  # coordinates(pointDF)<-~pointX+pointY
  # crs(pointDF)<-SOURCE
    
  polars <- ptransform(cbind(pointX, pointY, 0), SOURCE, TARGET, silent = FALSE)
  
  polars <- data.frame(polars)
  colnames(polars) <- c("X", "Y", "Z")
  
  #polars is a dataFrame
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
nc_close(fileFull)

  
    
  
cellSize<-x[[2]]-x[[1]] #constant size square grid is assumed in this projection
offsetGrid<-cellSize/2 #x,y coordinate of the grid are mid points


xmin<-min(x)
ymin<-min(y)

xcell<-ceiling((xvar-xmin+offsetGrid)/cellSize)#getting to the NetCDF array cell from the coordinate
ycell<-ceiling((yvar-ymin+offsetGrid)/cellSize)#

list(x=xcell, y=ycell)

}




getMetaDataNetCDF<-function(filepath, variableOfInterest = "image1_image_data"){
  fileFull<-nc_open(filepath)
  missingValue <- ncatt_get(fileFull,variableOfInterest,"_FillValue")
  missingValue <- missingValue$value
  scaleFactor <- ncatt_get(fileFull,variableOfInterest,"scale_factor")
  scaleFactor <- scaleFactor$value
  addOffsetFactor <- ncatt_get(fileFull,variableOfInterest,"add_offset")
  addOffsetFactor <- addOffsetFactor$value
  nc_close(fileFull)
  
  list(missingValue = missingValue, scaleFactor = scaleFactor, addOffset = addOffsetFactor, variable = variableOfInterest)
}


getMetaDataNetCDF(exampleNetCDFfile)


makeCellfunction <- function(filepath) {
  
  #loading NetCDF file 
  fileFull<-nc_open(filepath)
  
  #reading in the variables
  x<-ncvar_get(fileFull, "x")
  y<-ncvar_get(fileFull, "y")
  
  #close the NetCDF file
  nc_close(fileFull)
  
  
  cellSize <- x[[2]] - x[[1]] #constant size square grid is assumed in this projection
  
  if (cellSize != abs(y[[1]] - y[[2]])) stop("Grid is not square") # we may drop this test
  
  # note that the y-values can decrease as we go down from the top line (the first line number is one), 
  # as we assume y is largest in to of the figure, so y[[1]] - y[[2]] > 0
  # in some cases this may be opposite. 
  # x values increase if we go to the right

  offsetGrid <- cellSize / 2 #x,y coordinate of the grid are mid points
  
  xmin <- min(x)
  ymax <- max(y)
  
  function(xvar, yvar) {
    
    inPolarCoordinates <- convertToPolar(xvar, yvar)
    
# inPolarCoordinates$X may be up to "offsetGrid" smaller than xmin to fall in the first column
# inPolarCoordinates$Y may be up to "offsetGrid" larger than ymax to fall in the first row (top row)
    
    columnX <- ceiling((inPolarCoordinates$X - xmin + offsetGrid) / cellSize) # getting to the NetCDF array cell from the coordinate (X)

    if (y[[1]] < y[[2]]) 
      rowY<-ceiling((yvar-ymin+offsetGrid)/cellSize)
    else
      rowY <- ceiling((ymax - inPolarCoordinates$Y + offsetGrid) / cellSize) # and (Y)
    list(X = columnX, Y = rowY, xvar = xvar, yvar = yvar)
  }
}

transFormFunction <- makeCellfunction(exampleNetCDFfile)


transFormFunction(pointX, pointY)
