# example to find USGS tiles covering a set of polygons
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

  # original logic assumed that the lpc_pub_date should be added if it is valid. However, I found a
  # project where the lpc_pub_date was valid but was not included in the tile names.
  # WA_Olympic_Peninsula_2013...project_id = 6373, workunit_id = 6371
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
tileFolder <- "H:/T3_AirborneLidar"
clipFolder <- "G:/R_Stuff/LIDAR_Data/Clips"

sampleBuffer <- 150   # 100m buffer around polygons

showMaps <- TRUE
if (showMaps) library(mapview)

# this will need to be changed to the folder containing local copies of the USGS index files
setwd("G:/R_Stuff/PlotClipping")

# use a local copy of the USGS project index
setUSGSProjectIndex("WESM_1_13_2022.gpkg")

# read polygon features from several shapefiles and merge into a single layer
# unfortunately, the shapefiles don't have the same columns or column order
# these files are on local storage and are not included as part of the package
Aa <- st_read("H:/T3_GIS/Final_layout/WS_Aa_June24_2021.shp")
Ba <- st_read("H:/T3_GIS/Final_layout/WS _Ba_June24_2021.shp")
Ca <- st_read("H:/T3_GIS/Final_layout/WS_Ca_June24_2021.shp")
Da <- st_read("H:/T3_GIS/Final_layout/WS_Da_June24_2021.shp")

# keep only the geometries and add an identifier
Aa <- Aa[, "geometry"]
Aa$ID <- 1
Ba <- Ba[, "geometry"]
Ba$ID <- 2
Ca <- Ca[, "geometry"]
Ca$ID <- 3
Da <- Da[, "geometry"]
Da$ID <- 4

# merge features in each unit into fewer polygons
Aa_merged <- stars::st_rasterize(Aa[, "ID"], nx = 512, ny = 512)
Aa_merged <- sf::st_as_sf(Aa_merged, as_points = FALSE, merge = TRUE)
sf::st_make_valid(Aa_merged)

Ba_merged <- stars::st_rasterize(Ba[, "ID"], nx = 512, ny = 512)
Ba_merged <- sf::st_as_sf(Ba_merged, as_points = FALSE, merge = TRUE)
sf::st_make_valid(Ba_merged)

Ca_merged <- stars::st_rasterize(Ca[, "ID"], nx = 512, ny = 512)
Ca_merged <- sf::st_as_sf(Ca_merged, as_points = FALSE, merge = TRUE)
sf::st_make_valid(Ca_merged)

Da_merged <- stars::st_rasterize(Da[, "ID"], nx = 512, ny = 512)
Da_merged <- sf::st_as_sf(Da_merged, as_points = FALSE, merge = TRUE)
sf::st_make_valid(Da_merged)

polys <- Aa_merged
polys <- rbind(polys, Ba_merged)
polys <- rbind(polys, Ca_merged)
polys <- rbind(polys, Da_merged)
if (showMaps) mapview(list(polys, Aa, Ba, Ca, Da))

# buffer the polygons
polys_buffer <- prepareTargetData(aoi = polys, buffer = sampleBuffer)

# merge adjacent polygons together
polys_merged <- stars::st_rasterize(polys_buffer[, "ID"], nx = 512, ny = 512)

# convert back to polygons...merge tiles
polys_aoi <- sf::st_as_sf(polys_merged, as_points = FALSE, merge = TRUE)
sf::st_make_valid(polys_aoi)

if (showMaps) mapview(polys_aoi)

# query for the project polygon containing the sample area
# defined by the center points and buffer...return is project area(s)
# that intersect the rectangular area
# this call is not needed for this example but useful for mapping
polys1 <- queryUSGSProjectIndex(aoi = polys_aoi, verbose = TRUE, lidarOnly = TRUE)

# now query to get the rectangular sample area attributed with the project
# polygon information...same call as above with return="aoi"
# return is the square sample area attributed with project information
aoi1 <- queryUSGSProjectIndex(aoi = polys_aoi, verbose = TRUE, lidarOnly = TRUE, return = "aoi")

if (showMaps) mapview(list(polys1, aoi1))

# use a local copy of the USGS tile index...this is a large file so I tend to always use a local copy
# there may be problems with some projects in the tile index. For this example, I know the index is good.
setUSGSTileIndex("LPC_TESM_1_13_2022.gpkg")

# need to run this using each aoi polygon since the URLs rely on the lpc_link for each project covering the aoi
URLs <- list()
tileList <- data.frame()
for (i in 1:nrow(aoi1)) {
  # query for tiles covering the aoi
  # using return = "aoi" returns geometry for the intersection of the tiles with the aoi
  # using return = "index" returns geometry for the full tile (complete record from tile index)...nothing to relate back to aoi
  tiles <- queryUSGSTileIndex(projectID = aoi1$workunit_id[i]
                              , fieldname = "workunit_id"
                              , aoi = aoi1[i, "ID"]
                              , return = "aoi"
  )
  tileList <- rbind(tileList, tiles)

  # drop tiles with tile_id = NA
  tiles <- tiles[!is.na(tiles$tile_id), ]

  if (length(tiles)) {
    # build URLs for tiles
    tURLs <- makeURLForTiles(aoi1[i, ], tiles)

    # add to overall list
    URLs <- append(URLs, tURLs)

    # fetch the tiles
    #  results <- fetchUSGSTiles(tileFolder, tURLs)
  }
}

# the list may have duplicates
URLs <- unique(URLs)

# fetch the tiles for the first project that follows the naming logic
res1 <- fetchUSGSTiles(tileFolder, URLs[1:30])  # first project

# the files for the second project don't follow the "standard" naming rules so we need to strip
# off part of the file name
tURLs <- URLs[31:35]
tURLs <- stringr::str_replace(tURLs, "_LAS_2020", "")

res2 <- fetchUSGSTiles(tileFolder, tURLs) # second project...LAZ file names not formed the same as the first project


# one tile was missed...I had one tile with a bad tile_id (NA). I suspect this was it so I manually build the URL
# and fetched it separately
# ran loop above with 1:1 and then used the first URL
# "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/USGS_LPC_WA_Olympic_Peninsula_2013_LAS_2015/laz/USGS_LPC_WA_Olympic_Peninsula_2013_10TDT200900_LAS_2015.laz"
URL <- str_replace(URLs[1], "10TDT200900", "10TDT180910")
res <- fetchUSGSTiles(tileFolder, URL)

# *****************************************************************************
# *****************************************************************************
# code to combine treatment units and reproject to 26910 (UTM10)
Aa <- st_read("H:/T3_GIS/Final_layout/WS_Aa_June24_2021.shp")
Ba <- st_read("H:/T3_GIS/Final_layout/WS _Ba_June24_2021.shp")
Ca <- st_read("H:/T3_GIS/Final_layout/WS_Ca_June24_2021.shp")
Da <- st_read("H:/T3_GIS/Final_layout/WS_Da_June24_2021.shp")

Canew <- Ca[, c(1:11, 13)]

Aanew <- Aa[, as.vector(colnames(Canew))]
Banew <- Ba[, as.vector(colnames(Canew))]
Danew <- Da[, as.vector(colnames(Canew))]

# add block identifier
Aanew$ID <- "Aa"
Banew$ID <- "Ba"
Canew$ID <- "Ca"
Danew$ID <- "Da"

# merge units
units <- rbind(Aanew, Banew, Canew, Danew)
mapview(units, zcol = "ID", burst = TRUE)

# reproject
st_crs(units)
units_UTM10 <- st_transform(units, crs = 26910)
st_crs(units_UTM10)

st_write(units_UTM10, "H:/T3_GIS/Final_layout/Units_UTM10.shp")
st_write(units_UTM10, "H:/T3_GIS/Final_layout/Units_UTM10.gpkg")
