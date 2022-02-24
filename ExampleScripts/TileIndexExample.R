# code demonstrating the use of the USGS tile index to find and clip data for sample locations
#
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

# clip a square sample using the bounding box of the aoi features using FUSION's clipdata program.
# *****this requires a working version of FUSION installed and the install folder on the path
#
# want to add ability to write clip commands to a batch file for execution from CMD prompt
clipPoints <- function (
  aoi,
  URLs,
  baseName,
  lastReturns = FALSE,
  inputFolder = "",
  destFolder = "",
  compress = TRUE
) {
  if (destFolder == "") {
    destFolder <- paste(getwd(), "/", sep = "")
  } else {
    # make sure folder exists
    if (!base::dir.exists(file.path(destFolder))) {
      if (!base::dir.create(file.path(destFolder)))
        stop("could not create destFolder: ", destFolder)
    }
  }

  lastFlag <- ""
  if (lastReturns) lastFlag <- "/return:L"

  # build list of input files...convert URLS to simple names, add folder
  inputFiles <- file.path(inputFolder, basename(URLs))

  # write list to temp file
  tempListFile <- file.path(inputFolder, "__templist.txt")
  readr::write_lines(inputFiles, tempListFile)

  # set extension
  ext <- ".laz"
  if (!compress) ext <- ".las"

  # build output file specifier
  outputFile <- file.path(destFolder, paste0(baseName, "_Clip", ext))

  # reproject aoi into coord system matching lidar data
  aoi <- sf::st_transform(aoi, as.integer(aoi$horiz_crs))

  # get bounding box for feature
  bb <- sf::st_bbox(aoi)

  # /shape:0 is for squares, /zero so we get empty files if no points
  cmd <- paste("clipdata64"
               , paste0("/shape:0  /zero")
               , lastFlag
               , tempListFile
               , outputFile
               , bb[1]
               , bb[2]
               , bb[3]
               , bb[4]
  )

  cat(cmd, "\n")
  ret <- shell(cmd)

  # delete temp list file
  unlink(tempListFile)

  return(ret)
}

# compute metrics for a point clip using FUSION's cloudmetrics program
# *****still a work in progress
# *****does not normalize points so metrics are useless unless point clips have been normalized
#
# want to add ability to write clip commands to a batch file for execution from CMD prompt
computeMetricsDO_NOT_USE <- function (
  plot,
  heightThreshold = 4.5,
  coverThreshold = 4.5,
  firstReturns = TRUE,
  inputFolder = "",
  inputFileTemplate = "*.las",
  outputFolder = "",
  outputFileBaseName = "metrics.csv",
  deleteClips = FALSE
) {
  first <- ""
  if (firstReturns) first <- "/first"

  cmd <- paste("cloudmetrics"
               , "/new"
               , paste0("/above:", coverThreshold)
               , paste0("/minht:", heightThreshold)
               , first
               , file.path(inputFolder, inputFileTemplate)
               , file.path(outputFolder, paste0(plot, "_", outputFileBaseName))
  )

  if (shell(cmd) == 0) {
    if (deleteClips) {
      unlink(inputFiles)
    }
  }
}

# *****************************************************************************
# *****************************************************************************

# options
tileFolder <- "G:/R_Stuff/LIDAR_Data/tempTiles"
clipFolder <- "G:/R_Stuff/LIDAR_Data/Clips"

sampleSize <- 100   # 100m square area
sampleBuffer <- 5   # 5m additional area...allows us to create ground model, normalize point data and clip to final area

showMaps <- TRUE
if (showMaps) library(mapview)

# this will need to be changed to the folder containing local copies of the USGS index files
setwd("G:/R_Stuff/PlotClipping")

# use a local copy of the USGS project index
setUSGSProjectIndex("WESM_10_14_2021.gpkg")

# construct a single point feature using web mercator location
# ID is used in the clipPoints function to build the file name for the point data
pt <- sf::st_sf(data.frame(ID = "P1", stringsAsFactors = F),
                geometry = sf::st_sfc(sf::st_point(c(-13499097, 6139669))),
                crs = 3857)

# project to UTM sone 10N, EPSG:26910
pt <- st_transform(pt, 26910)  # UTM zone 10N...26919=zone 19

# buffer the point to produce the sample area
pt_aoi <- prepareTargetData(aoi = pt, buffer = sampleSize / 2 + sampleBuffer)

if (showMaps) mapview(pt_aoi)

# query for the project polygon containing the sample area
# defined by the center points and buffer...return is project area(s)
# that intersect the rectangular area
# this call is not needed for this example but useful for mapping
polys1 <- queryUSGSProjectIndex(aoi = pt_aoi, verbose = TRUE, lidarOnly = TRUE)

# now query to get the rectangular sample area attributed with the project
# polygon information...same call as above with return="aoi"
# project is USGS_LPC_WA_GlacierPeak_2014_LAS_2016
# return is the square sample area attributed with project information
aoi1 <- queryUSGSProjectIndex(aoi = pt_aoi, verbose = TRUE, lidarOnly = TRUE, return = "aoi")

if (showMaps) mapview(list(polys1, aoi1))

# use a local copy of the USGS tile index...this is a large file so I tend to always use a local copy
# there may be problems with some projects in the tile index. For this example, I know the index is good.
setUSGSTileIndex("LPC_TESM_10_14_2021.gpkg")

# need to run this using each aoi polygon since the URLs rely on the lpc_link for each project covering the aoi
# for this example, only 1 project is involved so the loop isn't really needed
for (i in 1:nrow(aoi1)) {
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

  # do the clip...change folder names to reflect your local folder structure
  clipPoints(aoi1[i, ]
             , URLs
             , aoi1$ID[i]
             , inputFolder = tileFolder
             , destFolder = clipFolder
  )
}

# at this point, we have clips for the areas in aoi1. To compute metrics for the points, we need to create a ground model
# (hopefully using class 2 points) and call CloudMetrics using the computeMetrics function. However, the computeMetrics function
# is not ready for use...don't use it!!

# idea is to build ground model using class 2 points, then use ClipData to normalize the points and clip back to the original sample area
# However, this code does not support these operations
