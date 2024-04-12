# code demonstrating the use of the MPC tile index to find and clip data for a sample area defined by bounding box
# To make it even simpler, the center point of the bounding box is used along with the "radius" for the sample area
# (1/2 the width of the bounding box)
# these examples are works in progress. the code may be useful but it has some things specific to
# the developers file system.
#
library(sf)
library(dplyr)
library(tidyverse)
library(lubridate)

library(USGSlidar)

# *****************************************************************************
# *****************************************************************************

# options
tileFolder <- "G:/R_Stuff/LIDAR_Data/tempTiles"
clipFolder <- "G:/R_Stuff/LIDAR_Data/Clips"

bbMinX <- 463980.000000
bbMinY <- 4047980.000000
bbMaxX <- 468020.000000
bbMaxY <- 4050020.000000

midX <- (bbMinX + bbMaxX) / 2
midY <- (bbMinY + bbMaxY) / 2
buffer <- (bbMaxX - bbMinX) / 2

epsg <- 6346

# this will need to be changed to the folder containing local copies of the USGS index files
setwd("G:/R_Stuff")

# query for tiles covering the aoi
# using return = "aoi" returns geometry for the intersection of the tiles with the aoi
# using return = "index" returns geometry for the full tile (complete record from tile index)...nothing to relate back to aoi
tiles <- queryMPCTileIndex(midX, midY, buffer
                           #, crs = sp::CRS(SRS_string="EPSG:6346")@projargs
                           , crs = epsg
                           , return = "aoi"
)

# build URLs for tiles
URLs <- tiles$URL

# fetch the tiles...change this to reflect your local folder structure
results <- fetchUSGSTiles(tileFolder, URLs[1])

# check the URLs
message(sum(results == 0, na.rm = TRUE), " out of ", length(URLs), " retreived or already available")

# at this point, we have tiles covering the aoi.
