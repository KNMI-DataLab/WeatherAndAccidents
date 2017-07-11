# ImportPrecipitationRadarData.R
# 
# The following R script is used to create a key value table tables 
# "Time", "X", "Y", "Precipitation"
# from KNMI provided NetCDF files 
# http://adaguc.knmi.nl/contents/datasets/productdescriptions/W_ADAGUC_Product_description_RADNL_OPER_R___25PCPRR_L3.html
#
# This program assumes the NetCDF files already to be downloaded (for instance from
# http://opendap.knmi.nl/knmi/thredds/catalog/radarprecipclim/RAD_NL25_RAC_MFBS_5min_NC/catalog.html
# ) and decompressed into the directory pointed to by the environment variable 
# "ProjectData" pointing to a share (part of a path at least), and from there in 
# directories "KNMI", "PrecipitationRadar", and "NetCDF", for instance ProjectData equals 
# "/Volumes/ProjectData", combined it would make /Volumes/ProjectData/KNMI/PrecipitationRadar/NetCDF/
# If the environment variable is not set, the data is assumed to be decompressed into 
# the path ProjectData/KNMI/PrecipitationRadar/ relative to where the program starts. 
# The envirionment variable also controls the output location. If not set
# the path is KNMI/PrecipitationRadar/RData/ relative to where the program starts. 
# Currently, the zip files offered by KNMI are organized per year and month (and [ignored] hour).  
# This structure is assumed by the program: the program first determines a list of all 
# directories one level up from "NetCDF", and iterates over each of them. 
# Assuming these directories represent data for separate years, the program first checks
# for the presence of a output dataset associated with that year. If it finds one, it 
# assumes that year is already processed and skips to the next directory (more about this below).
# If the program assumes the year is not yet processed, it lists all subdirectories one 
# level up which it assumes to represent months of the year. In principle, the program 
# processes each month in turn, saves the monthly data in a separate file and concatenates 
# all per-month data and saves the annual aagregate data as well. If the program finds a  
# per-month data file, it loads the data just for concatenation, so in the end, the complete 
# aggregate annual data can be saved. This way it is relatively easy to add extra data once 
# new data becomes available (could be done more efficiently though). 
# If data needs to be replaced, either the output datasets can be deleted, or the function
# check.file.exists below can be changed to return FALSE
#
# Currently the program uses the Parallel package to load the monthly NetCDF files in 
# parallel using the forking mechanism, so this might not work on windows machines. 

# It is unclear to me whether the time used in the file name is the beginning of the 
# 5-minute interval or the end. May not matter much, but anyway.

check.file.exists <- function(thePath) file.exists(thePath)

# uncomment the next line to replace all data
# check.file.exists <- function(thePath) FALSE


#
# load required packages
#
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(ncdf4))
suppressPackageStartupMessages(require(parallel))


# One can use the "ProjectData" environment variable to determine the output location

if (Sys.getenv("ProjectData") == "") {
  outputDirectory <- file.path("KNMI", "PrecipitationRadar", "RData") # could be simplified
} else 
  outputDirectory <- file.path(Sys.getenv("ProjectData"), 
                               "KNMI",
                               "PrecipitationRadar",
                               "RData")

#
# list the year-directories
#
theDirs <- list.dirs(path = file.path(ifelse(Sys.getenv("ProjectData") == "", 
                                             "ProjectData", 
                                             Sys.getenv("ProjectData")),
                                      "KNMI",
                                      "PrecipitationRadar",
                                      "NetCDF"),
                       full.names = TRUE, 
                       recursive = FALSE)

# debugging code
# theDir <- theDirs[[1]]
# theDir

# iterate over the years

for (theDir in theDirs) {

# Path to potential output file

  pathToYearFile <- file.path(outputDirectory,
                              paste("PrecipitationRadarData-", basename(theDir), ".Rdata", sep = ""))

# if file exists, skip to next...

  if (check.file.exists(pathToYearFile)) 
    next ;
    
  theMonthDirs <- list.dirs(path = theDir,
                            full.names = TRUE, 
                            recursive = FALSE)
  
# debugging code
#  theMonthDir <- theMonthDirs[[1]]
#  theMonthDir
  
  
# create annual RadarData by concatenating all monthly data

  RadarData <- bind_rows(
    lapply(
      theMonthDirs, 
      function(theMonthDir) {
        
# Path to potential output file

        pathToMonthFile <- file.path(outputDirectory,
                                     paste("PrecipitationRadarData-", basename(theDir), "-", basename(theMonthDir), ".Rdata", sep = ""))
        
# if file exists (but not the annual aggregate, which still has to be made), load the data...

        if (file.exists(pathToMonthFile))
          load(pathToMonthFile) 
        else {

# if the file does not exist, create it
# first tell the world what happens (and the time we started)

          print(paste(date(), theMonthDir))

# make a list of all NetCDF files (assumed *.nc) below the month directory
# this includes per day directories

          theFiles <- list.files(path = theMonthDir,
                                 pattern = glob2rx("*.nc"), 
                                 full.names = TRUE, 
                                 recursive = TRUE, 
                                 no.. = TRUE)

# debugging code
#          theFile <- theFiles[[1]]
        
          nCores <- detectCores()
          my.cluster <- makeForkCluster(nCores)

          RadarDataMonth <- bind_rows(
            parLapply(
              my.cluster, 
              theFiles, 
              function(theFile) {
                    
                timepoint <- as.POSIXct(str_extract_all(theFile, "_[[:digit:]]{8}[[:digit:]]{4}\\.")[[1]],
                                       format = "_%Y%m%d%H%M.",
                                       tz = "UTC")
            
# code lended from Andrea's "getMetaDataNetCDF"

                fileH <- nc_open(theFile)

                variableOfInterest <- "image1_image_data"
                missingValue <- ncatt_get(fileH, variableOfInterest, "_FillValue")
                missingValue <- missingValue$value
                scaleFactor <- ncatt_get(fileH, variableOfInterest, "scale_factor")
                scaleFactor <- scaleFactor$value
                addOffsetFactor <- ncatt_get(fileH, variableOfInterest, "add_offset")
                addOffsetFactor <- addOffsetFactor$value

                x <- ncvar_get(fileH, "x")
                y <- ncvar_get(fileH, "y")
                image_data <- ncvar_get(fileH, variableOfInterest)
  #                    
                nc_close(fileH)
 
  # we obtained a matrix of integers, mostly containing na's
  # note that this matrix first dimension is x, and the second is y
  # so in terms of i, and j, the y values are columns (j)
                
  # the next procedure is as follows
  # first image_data is converted into a data frame.
  # this data.frame has columns V1 to Vn with n the number of columns (this is the Y dimension)
  # this data.frame has row.names 1:m with m the number of rows (this is the X dimension)
  # we create the data.frame, 
  # set the X value to the numeric value of the row numbers and 
  # set the Start value equal to the time point
  # 
                dataFrame <- as.data.frame(image_data)
                dataFrame$X <- as.integer(rownames(dataFrame))
                dataFrame$Start <- timepoint

  # next we gather, basically stack, all columns starting with "V", including X and Start
                
                thisFrameRadarData <- gather(dataFrame, 
                                             key = "yLabel", value = "Precipitation", 
                                             starts_with("V")) %>%

  # Filter the NA's and the missing values, 

                  filter(!is.na(Precipitation),
                         Precipitation > 0,
                         Precipitation != missingValue) %>%

  # Compute Y as the numerical value of the column name without the "V"
                  
                  mutate(Y = as.integer(substring(yLabel, 2)),

  # calculate the actual measurement                  

                         Precipitation = scaleFactor * Precipitation + addOffsetFactor) %>%

  # Select the required data                  
                  
                  select(Start, X, Y, Precipitation)
                                  
                return(thisFrameRadarData)
              }))

  # Refresh the cluster (all leaked data, if any gone)

          stopCluster(my.cluster)

  # save the monthly data
                    
          save(RadarDataMonth, 
               file = pathToMonthFile)
        }
        
        return(RadarDataMonth)
      }))

  # save the annual data
    
  save(RadarData, 
       file = pathToYearFile)
  
}

warnings()
sessionInfo()
proc.time()
