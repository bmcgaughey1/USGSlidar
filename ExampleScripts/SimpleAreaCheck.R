# code demonstrating the use of the USGS tile index to find and clip data for a sample area defined by bounding box
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

# functions
# build a URL using the lpc_link field. This logic seems to work for many projects. However, the folder
# structure isn't always consistent so there may be cases where the constructed URLs are not valid.
makeURLForTiles <- function(
  aoi,
  tiles,
  folder = "laz",
  prefix = "USGS_LPC_",
  extension = ".laz"
) {
  # check for trailing slash in baseURL
  baseURL <- aoi$lpc_link
  if (!endsWith(baseURL, "/")) baseURL <- paste0(baseURL, "/")

  if (!is.na(aoi$lpc_pub_date)) {
    URL <- paste0(baseURL, folder, "/", prefix, aoi$workunit, "_", tiles$tile_id, "_LAS_", lubridate::year(aoi$lpc_pub_date),".laz")
  } else {
    URL <- paste0(baseURL, folder, "/", prefix, aoi$workunit, "_", tiles$tile_id, ".laz")
  }

  return(URL)
}

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

project <- "USGS_LPC_VA_FEMA_R3_Southwest_B_2016"
epsg <- 6346

showMaps <- TRUE
if (showMaps) library(mapview)

# this will need to be changed to the folder containing local copies of the USGS index files
setwd("G:/R_Stuff")

# use a local copy of the USGS project index
setUSGSProjectIndex("WESM_3_31_2023.gpkg")

# query for the project polygon containing the sample area
# defined by the center points and buffer...return is project area(s)
# that intersect the rectangular area
# this call is not needed for this example but useful for mapping
polys1 <- queryUSGSProjectIndex(midX, midY, buffer, crs = epsg, verbose = TRUE, lidarOnly = TRUE)
#polys1 <- queryUSGSProjectIndex(midX, midY, buffer, crs = sp::CRS(SRS_string="EPSG:6346")@projargs, verbose = TRUE, lidarOnly = TRUE)

if (showMaps) mapview(list(polys1))

target <- polys1[3,]

# now query to get the rectangular sample area attributed with the project
# polygon information...same call as above with return="aoi"
# return is the square sample area attributed with project information
aoi1 <- queryUSGSProjectIndex(midX, midY, buffer, crs = epsg, verbose = TRUE, lidarOnly = TRUE, return = "aoi")
#aoi1 <- queryUSGSProjectIndex(midX, midY, buffer, crs = sp::CRS(SRS_string="EPSG:6346")@projargs, verbose = TRUE, lidarOnly = TRUE, return = "aoi")

# manually set the correct project. this could be done using the project name in the code above but the attributes
# for the project in WESM don't match the name used for the folders on rockyweb. Folder name has "USGS_LPC_" but
# WESM does not have this prepended to the project name.
aoi1 <- aoi1[3, ]

if (showMaps) mapview(list(polys1, aoi1))

# use a local copy of the USGS tile index...this is a large file so I tend to always use a local copy
# there may be problems with some projects in the tile index. For this example, I know the index is good.
setUSGSTileIndex("H:/Backup/R_Stuff/PlotClipping/LPC_TESM_3_9_2022.gpkg")

# need to run this using each aoi polygon since the URLs rely on the lpc_link for each project covering the aoi
# for this example, only 1 project is involved so the loop isn't needed
i <- 1
#for (i in 1:nrow(aoi1)) {
  # query for tiles covering the aoi
  # using return = "aoi" returns geometry for the intersection of the tiles with the aoi
  # using return = "index" returns geometry for the full tile (complete record from tile index)...nothing to relate back to aoi
  tiles <- queryUSGSTileIndex(projectID = aoi1$workunit_id[i]
                              , fieldname = "workunit_id"
                              , aoi = aoi1[i, "ID"]
                              , return = "aoi"
  )

  # build URLs for tiles
  URLs <- makeURLForTiles(aoi1[i, ], tiles)

  # fetch the tiles...change this to reflect your local folder structure
  results <- fetchUSGSTiles(tileFolder, URLs)

  # check the URLs
  message(sum(results == 0, na.rm = TRUE), " out of ", length(URLs), " retreived or already available")
#}

# at this point, we have tiles covering the aoi.
