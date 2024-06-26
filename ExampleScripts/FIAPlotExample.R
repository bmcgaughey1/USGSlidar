#
# example use of USGSlidar package with FIA plots
#
library(sf)
library(mapview)
library(RSQLite)
library(dplyr)
library(tidyverse)
library(lubridate)

library(USGSlidar)

setwd("G:/R_Stuff/FIAExample")

state <- "TN"
clipSize <- 1000
showMaps <- TRUE

useEvalidator <- FALSE
if (useEvalidator) {
  # code from Jim Ellenwood to read PLOT data from Evalidator
  # works as of 9/30/2021 but the API may go away in the future
  #
  # 4/12/2024 no longer works
  library(httr)
  library(jsonlite)

  # get a table of state numeric codes to use with Evalidator
  form <- list('colList'= 'STATECD,STATE_ABBR', 'tableName' = 'REF_RESEARCH_STATION',
                'whereStr' = '1=1',
                'outputFormat' = 'JSON')
  urlStr <- 'https://apps.fs.usda.gov/Evalidator/rest/Evalidator/refTablePost?'
  response <- POST(url = urlStr, body = form, encode = "form") #, verbose())
  states <- as.data.frame(fromJSON(content(response, type="text")))
  states<-rename_with(states, ~ gsub("FIADB_SQL_Output.record.", "", .x,fixed=TRUE), starts_with("FIADB"))
  statecd<-states[states$"STATE_ABBR" == state,"STATECD"]

  form = list('colList' = 'CN,PREV_PLT_CN,STATECD,INVYR,MEASYEAR,MEASMON,MEASDAY,LAT,LON,DESIGNCD,KINDCD,PLOT_STATUS_CD',
               'tableName' = 'PLOT', 'whereStr' = paste0('STATECD=', statecd),
               'outputFormat' = 'JSON')
  urlStr = 'https://apps.fs.usda.gov/Evalidator/rest/Evalidator/refTablePost?'
  response <- POST(url = urlStr, body = form, encode = "form") #, verbose())

  library(jsonlite)
  PLOT <- as.data.frame(fromJSON(content(response, type = "text")))

  library(dplyr)
  PLOT <- rename_with(PLOT, ~ gsub("FIADB_SQL_Output.record.","",.x,fixed=TRUE), starts_with("FIADB"))
} else {
  # get FIADB for state
  FIADBFile <- paste0("FIADB_", state, ".db")

  # check to see if file exists
  if (!file.exists(FIADBFile)) {
    fetchFile(paste0("https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_", state, ".zip"), paste0(state, ".zip"))

    # unzip the file
    unzip(zipfile = paste0(state, ".zip"), exdir = getwd())

    if (file.exists(FIADBFile)) {
      file.remove(paste0(state, ".zip"))
    } else {
      # report a problem unzipping the file
      cat("*****Could not unzip file: ", paste0(state, ".zip"), "\n")
    }
  }

  # connect to FIADB
  con <- dbConnect(RSQLite::SQLite(), FIADBFile)

  # read tables
  PLOT <- dbReadTable(con, "PLOT")

  # disconnect from database
  dbDisconnect(con)
}
totalPlots <- nrow(PLOT)

# get plots measured in last ~10 years...could include multiple measurements for some plots
PLOT <- subset(PLOT, MEASYEAR >= 2010)
plotsSince2010 <- nrow(PLOT)

# FIADB locations are NAD83 LON-LAT (EPSG:4269)...this is OK but I prefer to work in web mercator (EPSG:3857)
# create sf object with locations
pts_sf <- st_as_sf(PLOT,
                   coords = c("LON", "LAT"),
                   remove = FALSE,
                   crs = 4269)

# reproject to web mercator
pts_sf <- st_transform(pts_sf, 3857)

if (showMaps) mapview(pts_sf)

# retrieve the entwine index with metadata...the enhanced entwine index comes from a github site
# that is updated every 3 days.
fetchUSGSProjectIndex(type = "entwineplus")

# Compute buffer sizes to use to "correct" distortions related to web mercator projection.
# For this example, I use the plot locations in web mercator. The locations will be projected
# to NAD83 LON-LAT to get the latitude for each plot location. The buffer (1/2 plot size) will
# be different for every plot.
#
# NOTE: all calls to queryUSGS... functions will use the new buffers instead of clipSize / 2
plotBuffers <- computeClipBufferForCONUS(clipSize / 2, points = pts_sf)

# Query the index to get the lidar projects covering the points (including a buffer).
# This call returns the boundaries for lidar projects that contain plots.
projects_sf <- queryUSGSProjectIndex(buffer = plotBuffers, aoi = pts_sf)

# subset the results to drop the KY statewide aggregated point set.
# There are a few lidar projects like this where data from several projects
# were aggregated. Unfortunately, the naming isn't consistent so you have to
# know something about the data.
#
# This example was developed for Tennessee. For another state that doesn't border Kentucky,
# this line can be omitted. Indiana also has an aggregate project as well as individual projects
# for each county.
projects_sf <- subset(projects_sf, name != "KY_FullState")

if (showMaps) mapview(projects_sf)

# Query the index again to get the point data populated with lidar project information.
# This call returns the sample locations (polygons) with lidar project attributes.
real_polys_sf <- queryUSGSProjectIndex(buffer = plotBuffers, aoi = pts_sf, return = "aoi", shape = "square")

# query the index again to get the point data populated with lidar project information.
# This call returns the sample locations (points) with lidar project attributes.
real_pts_sf <- queryUSGSProjectIndex(buffer = 0, aoi = pts_sf, return = "aoi")

# Manipulate acquisition date information. Ideally, this functionality would be
# part of the USGSlidar package but there is no consistent format (column names)
# for the date information between different lidar data sources.
projects_sf <- subset(projects_sf, !is.na(collect_start) & !is.na(collect_end))
real_polys_sf <- subset(real_polys_sf, !is.na(collect_start) & !is.na(collect_end))
real_pts_sf <- subset(real_pts_sf, !is.na(collect_start) & !is.na(collect_end))

# Do the date math to allow filtering of the sample POLYGONS.
# This code is specific to the Entwine database structure populated using the PopulateEntwineDB.R code.
# Column names will be different for other index types.
# Build a datetime value for start and end of collection using lubridate functions.
real_polys_sf$startdate <- ymd(real_polys_sf$collect_start)
real_polys_sf$enddate <- ymd(real_polys_sf$collect_end)
real_polys_sf$avedate <- real_polys_sf$startdate + ((real_polys_sf$enddate - real_polys_sf$startdate) / 2)
real_polys_sf$measdate <- make_date(real_polys_sf$MEASYEAR, real_polys_sf$MEASMON, real_polys_sf$MEASDAY)

# Repeat the date math to allow filtering of the sample POINTS.
# Build a datetime value for start and end of collection using lubridate functions.
real_pts_sf$startdate <- ymd(real_pts_sf$collect_start)
real_pts_sf$enddate <- ymd(real_pts_sf$collect_end)
real_pts_sf$avedate <- real_pts_sf$startdate + ((real_pts_sf$enddate - real_pts_sf$startdate) / 2)
real_pts_sf$measdate <- make_date(real_pts_sf$MEASYEAR, real_pts_sf$MEASMON, real_pts_sf$MEASDAY)

# We only want plots measured within +- some time interval.
# Changed from 18 months to 30 months 2/24/2022.
old_dateHalfRange <- years(1) + months(6)
dateHalfRange <- years(2) + months(6)

# flag to control filtering behavior...
#   if TRUE, target plots and pts will include all plots in the new date range
#   if FALSE, target plots and pts will only include plots outside the old date range and within the new range
#
# For "normal" use, subsetBehavior should be TRUE
subsetBehavior <- TRUE
#subsetBehavior <- FALSE

if (subsetBehavior) {
  # do the date filtering on the POLYGONS
  target_polys_sf <- subset(real_polys_sf, (measdate - avedate) >= -dateHalfRange & (measdate - avedate) <= dateHalfRange)

  # do the date filtering on the POINTS
  target_pts_sf <- subset(real_pts_sf, (measdate - avedate) >= -dateHalfRange & (measdate - avedate) <= dateHalfRange)
} else {
  # do the date filtering on the POLYGONS
  target_polys_sf <- subset(real_polys_sf, ((measdate - avedate) > old_dateHalfRange & (measdate - avedate) <= dateHalfRange) |
                                           ((measdate - avedate) >= -dateHalfRange & (measdate - avedate) < -old_dateHalfRange))

  # do the date filtering on the POINTS
  target_pts_sf <- subset(real_pts_sf, ((measdate - avedate) > old_dateHalfRange & (measdate - avedate) <= dateHalfRange) |
                                       ((measdate - avedate) >= -dateHalfRange & (measdate - avedate) < -old_dateHalfRange))
}

if (showMaps) mapview(target_pts_sf)

cat("Total number of plots in FIADB for ", state, ": ", totalPlots, "\r\n",
  "Plots with measurements since 01/01/2010: ", plotsSince2010, "\r\n",
  "   Of these, ", nrow(pts_sf), " have lidar coverage", "\r\n",
  "   Of these, ", nrow(target_pts_sf), " have lidar coverage within ",
  year(dateHalfRange) * 12 + month(dateHalfRange),
  " months of field measurements", "\r\n"
)

# if we need a list of the lidar projects that cover the final target plot POLYGONS, we need to check to see if
# projects were dropped (because plots did not meet the measurement date requirements)
#
# We can simply subset the original set of projects based on some field from the set of plots that match the
# date criteria.
#
# NOTE: I am not using the project list for anything in this example
target_projects_sf <- subset(projects_sf, name %in% unique(target_polys_sf$name))

# set this to TRUE to use the template file that reprojects incoming data to web mercator (EPSG=3857)
#projectTo3857 <- TRUE
projectTo3857 <- FALSE

if (projectTo3857) {
  templateFile <- system.file("extdata", "plots_reproject.json", package = "USGSlidar", mustWork = TRUE)
} else {
  templateFile <- system.file("extdata", "plots.json", package = "USGSlidar", mustWork = TRUE)
}

# build PDAL pipelines...use template included with library...basic clip with no additional processing
buildPDALPipelineENTWINE(target_polys_sf,
                         IDColumnLabel = "CN",
                         URLColumnLabel = "url",
                         pipelineOutputFolder = "G:/R_Stuff/FIAExample/pipelines",
                         pipelineOutputFileBaseName = "Plot",
                         pipelineTemplateFile = templateFile,
                         clipOutputFolder = "G:/R_Stuff/FIAExample/clips",
                         verbose = 500
)

# build the output file name for clips and add a column to the data before writing to a file
# this is the logic used in buildPDALPipelineENTWINE() to build the output name using the
# values for URLColumnLabel and IDColumnLabel
lasFile <- paste0(basename(dirname(target_polys_sf$url)),
                  "_",
                  target_polys_sf$CN,
                  ".laz"
)

# add the clip file name to data
target_polys_sf$ClipFile <- lasFile

# write off data after dropping geometry
# NOTE: Excel doesn't really like some of the values in the file (CN & dates) so you are better off reading the
# data in R but you need to use the following options for read.csv:
#   stringsAsFactors = FALSE,
#   colClasses = c("CN"="character", "SRV_CN"="character", "CTY_CN"="character", "PREV_PLT_CN"="character")
write.csv(st_drop_geometry(target_polys_sf), paste0(state, "Plots.csv"))

# notes specific to the author's computer configuration:
#
# to run the pipelines, open a miniconda prompt and activate pdalworking environment,
# then run the RunME.bat file in the pipelines folder
#
# examples:
# pdal pipeline G:\R_Stuff\FIAExample\pipelines\Plot_473338115489998.json
# pdal pipeline G:\R_Stuff\FIAExample\pipelines\Plot_196883321010854.json

