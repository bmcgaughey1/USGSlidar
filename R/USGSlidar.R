#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           Ctrl + Shift + B
#   Check Package:             Ctrl + Shift + E
#   Test Package:              Ctrl + Shift + T
#   Build documentation:       Ctrl + Shift + D...not working
#   Build vignette:            Ctrl + Shift + K
#
# setwd("G:/R_Stuff/USGSlidar")
#
# To build documentation:
# devtools::document()

# set up local environment to hold global variables
USGSEnv <- new.env(parent = emptyenv())

USGSEnv$USGSProjectIndex <- ""
USGSEnv$USGSTileIndex <- ""
USGSEnv$USGSLoadedProjectIndex <- ""
USGSEnv$USGSProjectsWebMerc <- ""

# ---------- fetchFile
#
#' USGS Lidar Toolkit -- Retrieve a file
#'
#' This is a helper function used in the USGSlidar package to call
#' \code{utils::download.file()} to retrieve individual index files and data
#' tiles.
#'
#' @param url A character string with the URL for the file.
#' @param destfile A character string specifying the file name for the
#'   downloaded file.
#' @param method Method used with \code{download.file()}. Refer to \code{download.file()}
#'   documentation for possible methods.
#' @param mode A character string indicating the mode with which to write the
#'   file. Refer to \code{download.file()} documentation for possible modes.
#' @param ... Additional arguments passed to \code{download.file()}.
#' @return An (invisible) integer code, 0 for success and non-zero for failure.
#'   This is the value returned from \code{download.file}.
#' @examples
#' \dontrun{
#' fetchFile("sample", "ftp://somewebsite.com/files/samplefile.bin")
#' }
#' @export
fetchFile <- function(
  url,
  destfile,
  method = "libcurl",
  mode = "wb",
  ...
) {
  # assume everything is good and just download the url to destfile
  ret <- tryCatch(utils::download.file(url, destfile, method = method, mode = mode, ...),
                  error = function(e) 1)
  invisible(ret)
}


# ---------- fetchUSGSProjectIndex
#
#' USGS Lidar Toolkit -- Download Lidar Project Index
#'
#' Retrieve the USGS lidar project index file. The project index file
#' contains project area boundaries for all USGS lidar data. Additional
#' attributes provide information about the acquisition such as the
#' starting and ending collection dates, pulse density, quality level,
#' work unit identifier and the work package identifier. The index is
#' stored in geopackage format and can be somewhat large. The index
#' is refreshed whenever new data is added to the USGS collection.
#'
#' @details You can also download the project index manually and then use
#'   \code{setUSGSProjectIndex()} to activate the index for use with \code{USGSlidar}
#'   functions.
#'
#' @param destfile A character string specifying the file name for the downloaded
#'   index. If not provided or empty, the index is stored in the current
#'   working directory using the filename in the url.
#' @param url A character string with the URL for the project index file. If an empty
#'   string, the URL for the project index corresponding \code{type} is fetched.
#'   When \code{type = "FESM"}, the URL for FESM_LPC_PROJ.gpkg is used. The FESM index
#'   has not been updated since May 2020. The WESM index should be used for nearly all work.
#'   When \code{type = "WESM"}, the URL for WESM.gpkg is used. This is the default.
#'   When \code{type = "ENTWINE"} is used, the URL for the USGS Entwine data index
#'   maintained by Howard Butler is used.
#'   When \code{type = "ENTWINEPLUS"} is used, the URL for the enhanced USGS Entwine data index
#'   produced by Robert J. McGaughey is used.
#' @param type A character string specifying the index to use. Valid values
#'   are \code{"WESM"}, \code{"FESM"}, \code{"ENTWINE"}, or \code{"ENTWINEPLUS"}.
#' @param method Method used with \code{download.file()}.  Refer to \code{download.file()}
#'   documentation for possible methods.
#' @param ... Additional arguments passed to \code{download.file()}.
#' @return An (invisible) integer code, 0 for success and non-zero for failure.
#'   This is the value returned from \code{download.file}.
#' @examples
#' \dontrun{
#' fetchUSGSProjectIndex()
#' fetchUSGSProjectIndex("Project.gpkg")
#' fetchUSGSProjectIndex("Project.gpkg", method = "wininet")
#' }
#' @export
fetchUSGSProjectIndex <- function(
  destfile = "",
  url = "",
  type = "WESM",
  method = "libcurl",
  ...
) {
  # sort out the URL
  if (url == "") {
    if (tolower(type) == "fesm") {
      # this is the old index last updated in May 2020
      #url <- "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/FullExtentSpatialMetadata/FESM_LPC_Proj.gpkg"

      # updated to rockyweb server
      url <- "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/FullExtentSpatialMetadata/FESM_LPC_PROJ.gpkg"
    } else if (tolower(type) == "wesm") {
      # old FTP server
      #url <- "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/metadata/WESM.gpkg"

      # new web server
      url <- "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/metadata/WESM.gpkg"
    } else if (tolower(type) == "entwine") {
      url <- "https://raw.githubusercontent.com/hobu/usgs-lidar/master/boundaries/resources.geojson"
    } else if (tolower(type) == "entwineplus") {
      url <- "https://raw.githubusercontent.com/bmcgaughey1/EntwineIndex/main/Index/ENTWINEBoundaries.gpkg"
    } else {
      stop("Invalid value for type.")
    }
    # https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/metadata/WESM.gpkg
    # https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/
}

  # sort out the destination file
  if (destfile == "") {
    # get name from URL
    t <- urltools::url_parse(url)
    destfile <- paste(getwd(), "/", basename(t$path), sep = "")
  }

  # download the index
  ret <- fetchFile(url, destfile, method = method, mode = "wb", ...)

  # check the return
  if (ret == 0) {
    # successful...save the local file for later use
    USGSEnv$USGSProjectIndex <- destfile
    USGSEnv$USGSLoadedProjectIndex <- ""
    USGSEnv$USGSProjectsWebMerc <- ""
  } else {
    # not successful...clear local file
    USGSEnv$USGSProjectIndex <- ""
    stop("Download failed.")
  }
  invisible(ret)
}

# ---------- setUSGSProjectIndex
#
#' USGS Lidar Toolkit -- Set Filename for the Lidar Project Index
#'
#' Set the filename and path for the local copy of the USGS lidar project
#' index file. This is used when you have already used fetchUSGSProjectindex
#' to download a project index. While the project index is frequently
#' updated by the USGS, the index is large and it may not be necessary
#' to download a new copy every time you want to download lidar data.
#'
#' @param index Character string with the local file name for the project index.
#' @return An (invisible) integer code, 0 for success and non-zero for failure.
#' @examples
#' \dontrun{
#' setUSGSProjectIndex("g:/working_data/ProjectIndex.gpkg")
#' }
#' @export
setUSGSProjectIndex <- function(
  index
) {
  if (index != "") {
    # make sure file exists
    if (file.exists(index)) {
      message("Project index file: ", index, " activated and ready for query")
      USGSEnv$USGSProjectIndex <- index
      USGSEnv$USGSLoadedProjectIndex <- ""
      USGSEnv$USGSProjectsWebMerc <- ""
      invisible(0)
    } else {
      USGSEnv$USGSProjectIndex <- ""
      USGSEnv$USGSLoadedProjectIndex <- ""
      USGSEnv$USGSProjectsWebMerc <- ""
      stop(paste("Project index file:", index, "does not exist."))
    }
  } else {
    USGSEnv$USGSProjectIndex <- ""
    invisible(1)
  }
}

# ---------- fetchUSGSTileIndex
#
#' USGS Lidar Toolkit -- Download Lidar Tile Index
#'
#' Downloads the USGS tile index file from the USGS rockyweb server. The tile
#' index is the LPC_TESM geopackage that is updated by the USGS as new lidar
#' data are added to the published collection of data. This file is large so it
#' usually makes sense to download it once and keep a local copy. However, the
#' files also changes daily so if you want the latest index, you should download
#' the index more frequently.
#'
#' @details You can also download the tile index manually and then use \code{setUSGSTileIndex()}
#'   to activate the index for use with \code{USGSlidar} functions.
#'
#' @param destfile A character string specifying the file name for the downloaded
#'    tile index. If not provided or empty, the index is stored in the current
#'   working directory using the filename in the url.
#' @param url A character string with the URL for tile index file. If an empty
#'   string, FESM_LPC_TILE.gpkg from the USGS rockyweb server is used.
#' @param method Method used with \code{download.file()}
#' @param ... Additional arguments passed to \code{download.file()}
#' @return An (invisible) integer code, 0 for success and non-zero for failure.
#'   This is the value returned from \code{download.file}.
#' @examples
#' \dontrun{
#' fetchUSGSTileIndex("")
#' fetchUSGSTileIndex("Tiles.gpkg")
#' fetchUSGSTileIndex("Tiles.gpkg", method = "wininet")
#' }
#' @export
fetchUSGSTileIndex <- function(
  destfile = "",
  url = "",
  method = "libcurl",
  ...
) {
  # sort out the URL
  if (url == "") {
    # old tile index last updated 7/27/2021
    #url <- "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/FullExtentSpatialMetadata/FESM_LPC_TILE.gpkg"

    # new index
    url <- "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/FullExtentSpatialMetadata/LPC_TESM.gpkg"
  }

  # sort out the destination file
  if (destfile == "") {
    # get name from URL
    t <- urltools::url_parse(url)
    destfile <- paste(getwd(), "/", basename(t$path), sep = "")
  }

  # download the index
  ret <- fetchFile(url, destfile, method = method, mode = "wb", ...)

  # check the return
  if (ret == 0) {
    # successful...save the local file for later use
    USGSEnv$USGSTileIndex <- destfile
  } else {
    # not successful...clear local file
    USGSEnv$USGSTileIndex <- ""
    stop("Download failed.")
  }
  invisible(ret)
}

# ---------- setUSGSTileIndex
#
#' USGS Lidar Toolkit -- Set Filename for the Lidar Tile Index
#'
#' Set the filename and path for the local copy of the USGS lidar tile
#' index file. This is used when you have already used fetchUSGSTileindex
#' to download a tile index. The tile index is quite large and it is
#' frequently updated by the USGS. However, it may not be necessary to
#' download a new copy every time you want to download lidar data.
#'
#' @param index Character string with the file name for the tile index.
#' @return An (invisible) integer code, 0 for success and non-zero for failure.
#' @examples
#' \dontrun{
#' setUSGSTileIndex("g:/working_data/TileIndex.gpkg")
#' }
#' @export
setUSGSTileIndex <- function(
  index
) {
  if (index != "") {
    # make sure file exists
    if (file.exists(index)) {
      message("Tile index file: ", index, " activated and ready for query")
      USGSEnv$USGSTileIndex <- index
      invisible(0)
    } else {
      USGSEnv$USGSTileIndex <- ""
      stop(paste("Tile index file:", index, "does not exist."))
    }
  } else {
    USGSEnv$USGSTileIndex <- ""
    invisible(1)
  }
}

# ---------- fetchUSGSTiles
#
#' USGS Lidar Toolkit -- Download Lidar Tiles
#'
#' Download one or more files (usually lidar data tiles) described
#' by a list of URLs and place them in a folder.
#'
#' @param destfolder A character string with the name of the folder
#'   where the downloaded tiles should be saved. If empty, tiles
#'   are saved in the current working directory. The folder name
#'   should not include a trailing slash. The folder will be created
#'   if it does not exist.
#' @param urls A list of character strings with the URLs for tiles to
#'   fetch.
#' @param refresh Logical value indicating files that already exist in
#'   \code{destfolder} should be fetched again. The default behavior is
#'   to skip existing files. For skipped files, the return value is set
#'   to \code{0} indicating a successful fetch. The default behavior can
#'   be problematic when you have \code{urls} that targets files in
#'   different folders but have the same file name.
#' @param method Method used with download.file()
#' @param ... Additional arguments passed to download.file()
#' @return An (invisible) atomic vector of integer codes corresponding to the
#'   \code{urls}. The values are those returned from the \code{download.file}
#'   function calls for each of the \code{urls}, 0 for success and non-zero
#'   for failure. You can easily count the number of \code{urls} successfully
#'   fetched using \code{sum(value == 0, na.rm = TRUE)}.
#' @examples
#' \dontrun{
#' fetchUSGSTiles("", URLlist)
#' }
#' @export
fetchUSGSTiles <- function(
  destfolder = "",
  urls,
  refresh = FALSE,
  method = "libcurl",
  ...
) {
  # sort out the folder
  if (destfolder == "") {
    destfolder <- paste(getwd(), "/", sep = "")
  } else {
    # make sure folder exists
    if (!base::dir.exists(file.path(destfolder))) {
      if (!base::dir.create(file.path(destfolder)))
        stop("could not create destfolder: ", destfolder)
    }

    # check for trailing slash
    if (destfolder[length(destfolder)] != "/")
      destfolder <- paste(destfolder, "/", sep = "")
  }

  # loop through tiles and fetch
  if (length(urls)) {
    # set up vector of return values
    ret <- vector("integer", length(urls))

    index <- 1
    for(url in urls) {
      t <- urltools::url_parse(url)
      destfile <- paste(destfolder, basename(t$path), sep = "")

      # check to see if the target file already exists
      if (!base::file.exists(destfile) || refresh)
        ret[index] <- fetchFile(url, destfile, method, ...)
      else
        ret[index] <- 0

      index <- index + 1
    }
    invisible(ret)
  } else {
    stop("No tiles to fetch.")
  }
}

# ---------- computeClipBufferForCONUS
#
#' USGS Lidar Toolkit -- Compute buffer size(s) to correct for distance
#' distortions associated with the web Mercator projection (EPSG:3857)
#'
#' The web Mercator projection does not maintain accurate distances. The
#' distortions increase as you move away from the equator so a correction
#' is needed to produce sample areas that are consistent in size for
#' a set of samples across a range latitudes.
#'
#' \code{computeClipBufferForCONUS} uses a correction multiplier developed
#' for latitudes ranging from 23-49 degrees by comparing 1000m squares
#' defined in the web Mercator projection to their sized when reprojected
#' into UTM. While this is still not a perfect solution, it provides
#' a correction factor that gets you very close to the desired sample
#' size.
#'
#' @param desiredBuffer A single numeric value defining the desired buffer
#'   size or a vector of numeric values with the desired buffer sizes. If a
#'   vector of sizes is provided, it must be the same length as the vector
#'   provided for \code{centerPointLatitude}.
#' @param centerPointLatitude A single numeric value or vector of values
#'   containing the latitude of the sample points.
#' @param points A \code{SpatialPoints*} or \code{sf} object containing
#'   the sample point locations. The object must contain a coordinate system
#'   definition corresponding to the web Mercator projection. The points
#'   will be projected into NAD83 lon-lat and the resulting latitude values
#'   will be used to compute the buffer size(s).
#' @return A single value that is the buffer width needed to produce the
#'   \code{desiredBuffer} or a vector of buffer widths needed to produce the
#'   \code{desiredBuffer} for each sample point.
#' @examples
#' \dontrun{
#' computeClipBufferForCONUS(1000, 41.5)
#' }
#' @export
computeClipBufferForCONUS <- function(
  desiredBuffer,
  centerPointLatitude = NULL,
  points = NULL
) {
  buf <- NULL

  # check for latitude value(s) or points
  if (is.null(centerPointLatitude) && is.null(points)) {
    stop("You must provide either a latitude value or a set of points")
    return(NULL)
  }

  if (!is.null(centerPointLatitude) && !is.null(points)) {
    stop("You can only provide latitude values or a set of points...not both")
    return(NULL)
  }

  # check for latitudes
  if (!is.null(centerPointLatitude)) {
    # check length of desiredBuffer
    if (length(desiredBuffer) > 1) {
      if (length(desiredBuffer) != length(centerPointLatitude)) {
        stop("desiredBuffer must contain a value for each centerPointLatitude")
        return(NULL)
      }
    }

    buf <- desiredBuffer * (1.0 + ((-0.3065 + 0.3780 * centerPointLatitude)^2 / 1000))
  }

  # check for points
  if (!is.null(points)) {
    t <- points
    if (inherits(points, "Spatial")) {
      t <- sf::st_as_sf(points)
    }

    if (length(desiredBuffer) > 1) {
      if (length(desiredBuffer) != nrow(t)) {
        stop("desiredBuffer must contain a value for each point")
        return(NULL)
      }
    }

    # project to NAD83 lon-lat
    t <- sf::st_transform(t, 4269)

    buf <- desiredBuffer * (1.0 + ((-0.3065 + 0.3780 * st_coordinates(t)[, 2])^2 / 1000))
  }
  return(buf)
}

# ---------- queryUSGSProjectIndex
#
#' USGS Lidar Toolkit -- Identify USGS Lidar Project Covering Area
#'
#' Intersect a set of features (points or polygons) against a lidar
#' project index to determine which projects provide coverage for the
#' features. This is purely a spatial overlay operation and does not
#' use the attribute data associated with the index.
#'
#' @details Finds all USGS lidar projects that cover the area-of-interest
#'   and associates the polygon attributes with those of the input object(s)
#'   to produce the return object.
#'
#'   When used with \code{(x,y)}, a data object is created containing a single
#'   point. The \code{buffer} is then applied and the resulting area is
#'   intersected with the project polygons.
#'
#'   Internally, all data are in the web mercator coordinate reference system
#'   (EPSG:3857). Various data objects are projected to this CRS to
#'   perform the intersection and then projected to desired CRS for the
#'   return object.
#'
#' @param x Location easting for the center of the area-of-interest.
#' @param y Location northing for the center of the area-of-interest.
#' @param buffer Distance in meters added or subtracted to \code{(x,y)}
#'   to create the area-of-interest. Can be 0 when used with \code{(x,y)} to
#'   return items providing coverage for point location(s). Can be a
#'   vector of values corresponding to the number of objects in \code{aoi}
#'   allowing a different buffer size for each object when \code{aoi} is a
#'   \code{Spatial*} or \code{sf} object. \code{buffer} can be negative with
#'   polygon features to reduce the area considered. However, you may end up
#'   with weird shapes if the distance is larger than the width of the polygon.
#'   Data in \code{(x,y)} and \code{aoi} are projected into the web mercator
#'   coordinate reference system before the buffer operation is done so the
#'   units for \code{buffer} are always meters. When \code{buffer = 0},
#'   \code{aoi} is point(s), and \code{return = "index"} the return type
#'   will be polygon(s). While this is useful to determine the lidar
#'   projects needed to provide coverage for plots, it doesn't allow you to
#'   maintain a set of points that are covered by one or more lidar projects.
#'   If you want the set of points, pass the set of all points as \code{aoi},
#'   set \code{buffer = 0}, and set \code{return = "aoi"} but realize that a
#'   specific sample area associated with a point may actually be covered by
#'   additional lidar projects than those that cover the point location or may
#'   be partially outside the lidar project polygon.
#' @param shape Character string describing the shape of the sample area in
#'   the case of point features or the shape applied to the buffer corners
#'   when using polygon features. Valid values are \code{"square"} or \code{"circle"}.
#' @param aoi \code{Spatial*} or \code{sf} object containing a point or polygon
#'   describing the area of interest. Can be points or polygons.
#' @param crs Valid \code{proj4string} string defining the coordinate
#'   reference system of \code{(x,y)}. \code{crs} is required when using
#'   \code{(x,y)}. This can also be a valid \code{SRS_string} for
#'   use with the sp::CRS() function where it will be provided as the \code{projargs}
#'   argument. \code{crs} is ignored when \code{aoi} is specified.
#' @param index Index file for USGS lidar projects. If not provided, an index
#'   previously specified by a call to \code{fetchUSGSProjectIndex} or
#'   \code{setUSGSProjectIndex} will be used. If not provided and you have
#'   not called \code{fetchUSGSProjectIndex} or \code{setUSGSProjectIndex}, the
#'   function displays a warning and returns.
#' @param segments Number of segments to use when generating a circular
#'   area of interest. When using a \code{SpatialPoint*} or \code{sf} object
#'   with \code{shape = "circle"}, set \code{segments} to a rather large value (60
#'   or higher) that is a multiple of 4. The \code{gBuffer} function from
#'   \code{rgeos} and \code{st_buffer} function from \code{sf} are used to
#'   build the sample areas and it accepts the number of segments in a quarter
#'   circle so small values for \code{segments} may not produce good circles.
#'   Values for \code{segments} that are not a multiple of 4 will not
#'   produce circles with the correct number of segments.
#' @param return Character string specifying the kind of shapes to be
#'   returned. Valid values are "\code{"aoi"} to return shapes for each
#'   object in the \code{aoi} or the object defined by \code{(x,y)} and \code{buffer}
#'   with additional attributes providing lidar project information and
#'   \code{"index"} (default) to return project shapes that provide coverage
#'   for the features in the \code{"aoi"} or the area defined by \code{(x,y)}
#'   and \code{buffer}. When project shapes are returned there is no \code{"aoi"}
#'   information included with the project shape attributes.
#' @param returnType Character string specifying the object type for the
#'   returned polygon object when \code{(x,y)} is used to specify the
#'   area-of-interest. Valid values are \code{"Spatial"} or \code{"sf"}.
#'   \code{returnType} is ignored when \code{aoi} is specified.
#' @param returncrs Valid \code{proj4string} string defining the coordinate
#'   reference system of the returned \code{SpatialPolygonsDataFrame} or
#'   \code{sf} object. A value of \code{"same"} will project the return object
#'   to the same coordinate reference system as \code{aoi} or to \code{crs} when
#'   used with \code{(x,y)}. This can also be a valid \code{SRS_string} for
#'   use with the sp::CRS() function where it will be provided as the \code{projargs}
#'   argument.
#' @param lidarOnly Boolean indicating that only lidar projects should be considered
#'   for the spatial overlay. For the USGS WESM index, setting \code{lidarOnly = TRUE}
#'   will include projects with the following values for the collection method:
#'   linear-mode lidar, Bathymetric LIDAR, Topobathymetric LIDAR, Geiger-mode LIDAR,
#'   Single Photon LIDAR. For other indexes, use the default, \code{lidarOnly = FALSE},
#'   to prevent errors.
#' @param dropNAColumns list of column names to test in the index for NA values.
#'   If any values in any of the columns are NA, the feature will be dropped
#'   from the index prior to the spatial overlay. Default is to consider all
#'   features in the index.
#' @param clean Boolean to enable a cleaning operation for the project polygons
#'   using a call to \code{st_buffer} with \code{dist = 0}.
#'   This operation can fix some, but not all, topology problems. If you see
#'   warnings about self-intersecting rings, you may need to do some additional
#'   cleaning on a local copy of the project index.
#'   As of 9/22/2021, I added some additional checks and removed the call to
#'   st_buffer. The operations invoked when \code{clean = TRUE} can take a long
#'   time so you may be better off loading the index and cleaning it outside
#'   of this library.
#' @param verbose Boolean to enable printing of status messages. This is really
#'   only useful for code debugging.
#' @param ... Additional arguments passed to \code{download.file}
#' @return A \code{SpatialPolygonsDataFrame} or \code{sf} object containing project
#'   polygon(s) and attribute(s) for lidar projects covering the specified area.
#'   Attributes for the \code{aoi} are placed first in the \code{data.frame}
#'   followed by attributes for the lidar project polygons.
#' @examples
#' \dontrun{
#' queryUSGSProjectIndex(-13540901, 5806426, 180, shape = "circle", crs = 3857)
#'   }
#' @export
queryUSGSProjectIndex <- function(
  x,
  y,
  buffer = 0,
  shape = "square",
  aoi = NULL,
  crs = "",
  index = "",
  segments = 60,
  return = "index",
  returnType = "sf",
  returncrs = "same",
  lidarOnly = FALSE,
  dropNAColumns = NULL,
  clean = FALSE,
  verbose = FALSE,
  ...
) {
  # turn off some warnings from rgdal...I think these are related to updates to PROJ
  # and a lag between various package updates and how the packages interact with PROJ
  rgdal::set_rgdal_show_exportToProj4_warnings(FALSE)
  rgdal::set_thin_PROJ6_warnings(TRUE)

  # assume we are working with sf objects. if aoi is not sf, convert to sf and set flag
  # indicating we need to convert back to Spatial*...not relevant if using (x,y)
  convertTosp <- FALSE
  if (!is.null(aoi)) {
    target <- aoi

    # check object type
    if (inherits(target, "Spatial")) {
      if (verbose) message("--Converting Spatial* object to sf object")
      target <- sf::st_as_sf(target)
      convertTosp <- TRUE
    }

    # transform to web mercator...need to do this before buffer so we can use meters as units
    if (verbose) message("--Projecting target features to web mercator")
    targetWebMerc <- sf::st_transform(target, 3857)
  } else {
    targetWebMerc <- ""
  }

  # prepare feature data for query...may be based on point (x,y) or
  # aoi (Spatial* or sf* object)
  if (verbose) message("--Preparing target data")
  targetWebMerc <- prepareTargetData(x, y, buffer, shape, targetWebMerc, crs, segments, returnType)

  if (is.null(aoi)) {
    # just in case returnType is not sf, convert
    if (inherits(targetWebMerc, "Spatial")) {
      targetWebMerc <- sf::st_as_sf(targetWebMerc)
      convertTosp <- TRUE
    }

    # have x,y but it could be in a projection other than web mercator
    if (verbose) message("--Projecting target feature to web mercator")
    targetWebMerc <- sf::st_transform(targetWebMerc, 3857)
  }

  # set attribute-geometry relationship to constant...all attributes represent the entire polygon
  sf::st_agr(targetWebMerc) <- "constant"

  # ***** temp
  #plot(targetWebMerc)

  # see if we have a specified project index
  if (index == "") {
    # if no specified index, check for local project index
    if (USGSEnv$USGSProjectIndex != "")
      index <- USGSEnv$USGSProjectIndex
    else {
      stop(paste("No local tile index...use fetchUSGSProjectIndex()",
        "to download an index or setUSGSProjectIndex() to provide",
        "local file name,"))
      return(NULL)
    }
  }

#  if (USGSEnv$USGSLoadedProjectIndex == "" || USGSEnv$USGSLoadedProjectIndexIsLidarOnly != lidarOnly) {
  if (USGSEnv$USGSLoadedProjectIndex == "") {
      # load the project index...default behavior is to read the first layer
    # might have to add call to st_layers() to get list of layers and then
    # read the first or let user add a layer name
    # USGS WESM files only have 1 layer as of 9/7/2021
    if (verbose) message("--Reading index")
    projects <- sf::st_read(dsn = index, stringsAsFactors = FALSE)

    # transform to web mercator
    if (verbose) message("--Projecting index to web mercator")
    projectsWebMerc <- sf::st_transform(projects, 3857)

    # set attribute-geometry relationship to constant...all attributes represent the entire polygon
    sf::st_agr(projectsWebMerc) <- "constant"

    # we have loaded the index...save it for subsequent use
    USGSEnv$USGSLoadedProjectIndex <- index
    USGSEnv$USGSProjectsWebMerc <- projectsWebMerc
  } else {
    message("Using pre-loaded project index")
    projectsWebMerc <- USGSEnv$USGSProjectsWebMerc
  }

  # filter for lidar-only projects
  if (lidarOnly) {
    if (verbose) message("--Filtering for lidar projects")
    countBefore <- nrow(projectsWebMerc)
    projectsWebMerc <- projectsWebMerc[projectsWebMerc$p_method == "linear-mode lidar" |
                                         projectsWebMerc$p_method == "Topobathymetric LIDAR" |
                                         projectsWebMerc$p_method == "Geiger-mode LIDAR" |
                                         projectsWebMerc$p_method == "Single Photon LIDAR", ]
    countAfter <- nrow(projectsWebMerc)
    message("Found ", countAfter, " lidar projects out of ", countBefore, " total projects")
  }

  # if dropNAColumns is valid, check for and remove and features that have NA
  # values in any columns listed in dropNAColumns
  if (length(dropNAColumns) > 0) {
    if (verbose) message("--Filtering columns with NA values")
    projectsWebMerc <- projectsWebMerc[stats::complete.cases(projectsWebMerc[, dropNAColumns, drop = TRUE]), ]
  }

  # do a buffer operation to clean up topology
  # 9/23/2021...this appears to be failing...hangs
  if (clean) {
    if (verbose) message("--Checking index polygons")

    # check geometries
    # empty
    emptyCount <- sum(is.na(st_dimension(projectsWebMerc)))
    if (emptyCount)
      message("Found", emptyCount, "features with empty geometries")

    # corrupt
    corruptCount <- sum(is.na(st_is_valid(projectsWebMerc)))
    if (corruptCount)
      message("Found", corruptCount, "features with corrupt geometries")

    # invalid
    invalidCount <- sum(any(na.omit(st_is_valid(projectsWebMerc)) == FALSE))
    if (invalidCount)
      message("Found ", invalidCount, " features with invalid geometries")

    if (emptyCount + corruptCount + invalidCount) {
      if (emptyCount) {
        stop(paste("Index contains features with empty geometries",
                   "that cannot be fixed...aborting"))
        return(NULL)
      } else {
        if (verbose) message("--Attempting to clean index polygons using st_make_valid")
        projectsWebMerc <- st_make_valid(projectsWebMerc)
        if (any(na.omit(st_is_valid(projectsWebMerc) == FALSE))) {
          if (verbose) message("--After st_make_valid there are still invalid geometries")
          if (verbose) message("--Attempting to clean index polygons using st_buffer")
          #projectsWebMerc <- sf::st_buffer(projectsWebMerc, 0.0)
        } else {
          if (verbose) message("--Cleaning successful!!!")
        }
      }
    } else {
      message("Nothing to clean, all geometries seem valid")
    }
  }

  # intersect AOI with index...return is AOI shapes and AOI attributes
  # first in set of fields (left side of columns)
  # if there are any problems with the project shapes, you will get an error
  # might be able to wrap this in try() stmt to gracefully report
  # any problems
  #message("Warning messages (if any) from proj4string() regarding comments can be ignored...")
  if (verbose) message("--Intersecting target objects with index polygons")
  prj <- sf::st_intersection(targetWebMerc, projectsWebMerc)

  # intersect index with AOI...return is AOI shapes with AOI attributes
  # on right side of columns
  #prj <- raster::intersect(projectsWebMerc, targetWebMerc)

  if (length(prj) == 0)
    stop("no project polygons overlap the target aoi")

  if (tolower(return) == "aoi") {
    # set return to target shapes with target attributes prepended
    # to project attributes
    shortlist <- prj
  } else if (tolower(return) == "index") {
    # set return to a list of project shapes with no target information
    shortlist <- projectsWebMerc[prj, ]
  } else {
    stop("invalid string for \"return\"": return)
  }

  # if we want to implement returncrs, now is the time to reproject the
  # outputs...target is always a Spatial* object at this point
  if (tolower(returncrs) == "same") {
    # figure out the type provided using aoi and crs
    if (inherits(aoi, "Spatial")) {
      # aoi is Spatial* object
      if (verbose) message("--Projecting results: case 1")
      shortlist <- sf::st_transform(shortlist, crs=sf::st_crs(sp::CRS(raster::crs(aoi, asText = T))))
    } else if (inherits(aoi, "sf")) {
      # aoi is sf object
      if (verbose) message("--Projecting results: case 2")
      shortlist <- sf::st_transform(shortlist, crs=sf::st_crs(aoi))
    } else {
      # (x.y) provided so use crs
      if (tolower(crs) != "") {
        if (verbose) message("--Projecting results: case 3")
        shortlist <- sf::st_transform(shortlist, crs = sf::st_crs(crs))
      }
    }
  } else {
    # use returncrs
    if (verbose) message("--Projecting results: case 4")
    shortlist <- sf::st_transform(shortlist, crs = returncrs)
  }

  # ***** temp
  #plot(shortlist)

  if (convertTosp) {
    if (verbose) message("--Converting results to Spatial object")
    shortlist <- sf::as_Spatial(shortlist)
  }

  return(shortlist)
}

# ---------- queryUSGSTileIndex
#
#' USGS Lidar Toolkit -- Identify USGS Lidar Tiles Covering Area
#'
#' Intersect a set of features (points or polygons) against a lidar
#' tile index to determine which tiles are needed to provide coverage for the
#' features. This involves both a spatial overlay operation and a database query.
#' The database query is used to limit the features used for the spatial
#' overlay. This dramatically increases the speed of the query when the
#' area of interest only involves a few lidar projects.
#'
#' @details Query the tile index to find tiles that intersect the \code{buffer}ed
#'   \code{(x,y)} point(s) or \code{buffer}ed feature(s) provided in \code{aoi}.
#'
#' @param x Location easting for the center of the area-of-interest.
#' @param y Location northing for the center of the area-of-interest.
#' @param buffer Distance in meters added or subtracted to \code{(x,y)}
#'   to create the area-of-interest. Can be 0 when used with \code{(x,y)} to
#'   return items providing coverage for point location(s). Can be a
#'   vector of values corresponding to the number of objects in \code{aoi}
#'   allowing a different buffer size for each object when \code{aoi} is a
#'   \code{Spatial*} or \code{sf} object. \code{buffer} can be negative with
#'   polygon features to reduce the area considered. However, you may end up
#'   with weird shapes if the distance is larger than the width of the polygon.
#'   Data in \code{(x,y)} and \code{aoi} are projected into the web mercator
#'   coordinate reference system before the buffer operation is done so the
#'   units for \code{buffer} are always meters.
#' @param projectID Character string or list of character strings
#'   containing the ID(s) for the lidar project(s). Typically obtained
#'   by calling \code{queryUSGSProjectIndex()}. You must provide at least one
#'   project identifier.
#' @param fieldname Character string containing the name of the field with
#'   matches for the \code{projectID} to be used when querying the tile index.
#' @param shape Character string describing the shape of the sample area in
#'   the case of point features or the shape applied to the buffer corners
#'   when using polygon features. Valid values are \code{"square"} or \code{"circle"}.
#' @param aoi \code{Spatial*} or \code{sf} object containing a point or polygon
#'   describing the area of interest. Can be points or polygons.
#' @param crs Valid \code{proj4string} string defining the coordinate
#'   reference system of \code{(x,y)}. \code{crs} is required when using
#'   \code{(x,y)}. \code{crs} is ignored when \code{aoi} is specified.
#'   \code{crs} can also be an EPSG code (numeric).
#' @param index Index file for USGS lidar tiles If not provided, an index
#'   previously specified by a call to \code{fetchUSGSProjectIndex} or
#'   \code{setUSGSProjectIndex} will be used. If not provided and you have
#'   not called \code{fetchUSGSTileIndex} or \code{setUSGSTileIndex}, the
#'   function displays a warning and returns.
#' @param segments Number of segments to use when generating a circular
#'   area of interest. When using a \code{SpatialPoint*} or \code{sf} object
#'   with \code{shape = "circle"}, set \code{segments} to a rather large value (60
#'   or higher) that is a multiple of 4. The \code{gBuffer} function from
#'   \code{rgeos} and \code{st_buffer} function from \code{sf} are used to
#'   build the sample areas and it accepts the number of segments in a quarter
#'   circle so small values for \code{segments} may not produce good circles.
#'   Values for \code{segments} that are not a multiple of 4 will not
#'   produce circles with the correct number of segments.
#' @param return Character string specifying the kind of shapes to be
#'   returned. Valid values are "\code{"aoi"} to return shapes for each
#'   object in the \code{aoi} or the object defined by \code{(x,y)} and \code{buffer}
#'   with additional attributes providing lidar project information and
#'   \code{"index"} (default) to return tile shapes that provide coverage
#'   for the features in the \code{"aoi"} or the area defined by \code{(x,y)}
#'   and \code{buffer}. When tile shapes are returned there is no \code{"aoi"}
#'   information included with the project shape attributes.
#' @param returnType Character string specifying the object type for the
#'   returned polygon object when \code{(x,y)} is used to specify the
#'   area-of-interest. Valid values are \code{"Spatial"} or \code{"sf"}.
#'   \code{returnType} is ignored when \code{aoi} is specified.
#' @param returncrs Valid \code{proj4string} string defining the coordinate
#'   reference system of the returned \code{SpatialPolygonsDataFrame} or
#'   \code{sf} object. A value of \code{"same"} will project the return object
#'   to the same coordinate reference system as \code{aoi} or to \code{crs} when
#'   used with \code{(x,y)}.
#' @param verbose Boolean to enable printing of status messages. This is really
#'   only useful for code debugging.
#' @param ... Additional arguments passed to \code{download.file}
#' @return A \code{SpatialPolygonsDataFrame} or \code{sf} object containing
#'   polygon(s) and attribute(s) for tiles covering the specified area.
#'   Attributes for the \code{aoi} are placed first in the \code{data.frame}
#'   followed by attributes for the lidar tile polygons..
#' @examples
#' \dontrun{
#' queryUSGSTileIndex(-13540901, 5806426, 180, shape = "circle", crs = 3857)
#'   }
#' @export
queryUSGSTileIndex <- function(
  x,
  y,
  buffer = 0,
  projectID = NULL,
  fieldname = "workunit_id",
  shape = "square",
  aoi = NULL,
  crs = "",
  index = "",
  segments = 60,
  return = "index",
  returnType = "sf",
  returncrs = "same",
  verbose = FALSE,
  ...
) {
  # I could add the clean option that is used for the project index. However, I
  # do not think there should be topology problems with the tiles since they should
  # represent more "friendly" shapes than the project polygons. The cleaning is
  # easy to add so I will wait...NOT!!!! (the easy part...it is only easy if it works)
  #
  # turn off some warnings from rgdal...I think these are related to updates to PROJ
  # and a lag between various package updates and how the packages interact with PROJ
  rgdal::set_rgdal_show_exportToProj4_warnings(FALSE)
  rgdal::set_thin_PROJ6_warnings(TRUE)

  # check that we have something in the projectID
  if (!length(projectID)) {
    stop("You must provide at least one project identifier in projectID.")
  }

  # assume we are working with sf objects. if aoi is not sf, convert to sf and set flag
  # indicating we need to convert back to Spatial*...not relevant if using (x,y)
  convertTosp <- FALSE
  if (!is.null(aoi)) {
    target <- aoi

    # check object type
    if (inherits(target, "Spatial")) {
      if (verbose) message("--Converting Spatial* object to sf object")
      target <- sf::st_as_sf(target)
      convertTosp <- TRUE
    }

    # transform to web mercator...need to do this before buffer so we can use meters as units
    if (verbose) message("--Projecting target features to web mercator")
    targetWebMerc <- sf::st_transform(target, 3857)
  } else {
    targetWebMerc <- ""
  }

  # prepare feature data for query...may be based on point (x,y) or
  # aoi (Spatial* or sf* object)
  if (verbose) message("--Preparing target data")
  targetWebMerc <- prepareTargetData(x, y, buffer, shape, targetWebMerc, crs, segments, returnType)

  if (is.null(aoi)) {
    # just in case returnType is not sf, convert
    if (inherits(targetWebMerc, "Spatial")) {
      targetWebMerc <- sf::st_as_sf(targetWebMerc)
      convertTosp <- TRUE
    }

    # have x,y but it could be in a projection other than web mercator
    if (verbose) message("--Projecting target feature to web mercator")
    targetWebMerc <- sf::st_transform(targetWebMerc, 3857)
  }

  # set attribute-geometry relationship to constant...all attributes represent the entire polygon
  sf::st_agr(targetWebMerc) <- "constant"

  # ***** temp
  #plot(targetWebMerc)

  # see if we have a specified project index
  if (index == "") {
    # if no specified index, check for local project index
    if (USGSEnv$USGSTileIndex != "")
      index <- USGSEnv$USGSTileIndex
    else {
      stop(paste("No local tile index...use fetchUSGSTileIndex()",
        "to download an index or setUSGSTileIndex() to provide",
        "local file name,"))
      return(NULL)
    }
  }

  # Because the tile index is so large, we only want to read tiles
  # for the specific project
  if (USGSEnv$USGSTileIndex == "") {
    if (index == "")
      stop("You must provide the file name for the tile index or call ",
        "one of the other TileIndex functions in the package to cache ",
        "the name of the tile index.")

    TileIndex <- index
  } else {
    TileIndex <- USGSEnv$USGSTileIndex
  }

  # this code (at least the FieldName) will need to change once USGS
  # gets their act sorted out.
  USGSTileLayer <- (sf::st_layers(TileIndex))$name[1]
  #FieldName <- "project_id"

  # read tiles and build index for specific project
  # load the tile index...default behavior is to read the first layer
  # might have to add call to st_layers() to get list of layers and then
  # read the first or let user add a layer name
  # USGS files only have 1 layer as of 6/29/2020
  #
  # use a query to only load tiles for the specified project
  #
  # have to use read_sf to use SQL query directly. readOGR does not
  # support SQL but this link has a way to subset:
  # https://geospatial.commons.gc.cuny.edu/2013/12/31/subsetting-in-readogr/
  #
  shortlist <- NULL
  count <- 1
  for (projID in projectID) {
    if (verbose) message("--Querying tile index for", projID)
    tiles <- sf::read_sf(dsn = TileIndex,
      stringsAsFactors = FALSE,
      query = sprintf("SELECT * FROM \"%s\" WHERE \"%s\" = '%s'",
        USGSTileLayer,
        fieldname,
        projID)
      )

    # make sure we have some features
    if (nrow(tiles) == 0) {
      message("no tiles found where ", fieldname, " is ", projID)
    } else {
      # transform to web mercator
      if (verbose) message("--Projecting tiles to web mercator")
      tilesWebMerc <- sf::st_transform(tiles, 3857)

      # we have loaded the index...save it for subsequent use
      # we only save the name since the tile index is large
      USGSEnv$USGSTileIndex <- TileIndex

      # set attribute-geometry relationship to constant...all attributes represent the entire polygon
      sf::st_agr(tilesWebMerc) <- "constant"

      # intersect AOI with index...return is SpatialPolygonsDataFrame
      #message("Warning messages (if any) from proj4string() regarding comments can be ignored...")
      prj <- sf::st_intersection(targetWebMerc, tilesWebMerc)

      if (length(prj) == 0) {
        message("no tiles overlap the target aoi for projectID: ", projID)
      } else {
        # status message
        message("Found ", nrow(prj), " tiles where ", fieldname, " is ", projID)

        if (tolower(return) == "aoi") {
          # set return to target shapes with target attributes prepended
          # to project attributes
          if (count == 1)
            shortlist <- prj
          else
            shortlist <- rbind(shortlist, prj, makeUniqueIDs = TRUE)
        } else if (tolower(return) == "index") {
          # set return to a list of tile shapes with no target information
          if (count == 1)
            shortlist <- tilesWebMerc[prj, ]
          else
            shortlist <- rbind(shortlist, tilesWebMerc[prj, ], makeUniqueIDs = TRUE)
        } else {
          stop("invalid string for \"return\"": return)
        }
        count <- count + 1
      }
    }
  }

  if (!is.null(shortlist)) {
    if (nrow(shortlist) > 0) {
      # if we want to implement returncrs, now is the time to reproject the
      # outputs...target is always a Spatial* object at this point
      if (tolower(returncrs) == "same") {
        # figure out the type provided using aoi and crs
        if (inherits(aoi, "Spatial")) {
          # aoi is Spatial* object
          if (verbose) message("--Projecting results: case 1")
          shortlist <- sf::st_transform(shortlist, crs=sf::st_crs(sp::CRS(raster::crs(aoi, asText = T))))
        } else if (inherits(aoi, "sf")) {
          # aoi is sf object
          if (verbose) message("--Projecting results: case 2")
          shortlist <- sf::st_transform(shortlist, crs=sf::st_crs(aoi))
        } else {
          # (x,y) provided so use crs
          if (tolower(crs) != "") {
            if (verbose) message("--Projecting results: case 3")
            shortlist <- sf::st_transform(shortlist, crs = sf::st_crs(crs))
          }
        }
      } else {
        # use returncrs
        if (verbose) message("--Projecting results: case 4")
        shortlist <- sf::st_transform(shortlist, crs = returncrs)
      }

      # ***** temp
      #plot(shortlist)

      if (convertTosp) {
        if (verbose) message("--Converting results to Spatial object")
        shortlist <- sf::as_Spatial(shortlist)
      }
    } else {
      shortlist <- NULL
    }
  }

  return(shortlist)
}

# ---------- clearUSGSProjectIndex
#
#' USGS Lidar Toolkit -- Clear the Stored Project Index File Name and
#' Boundary Cache
#'
#' Clear the name of the local project index file and remove the project
#' boundaries if they have been loaded. The loaded project boundaries
#' amount to a large amount of data. If you plan to save your workspace,
#' it is a good idea to call clearUSGSProjectIndex() to remove the
#' boundaries from memory. Otherwise expect large workspace files and
#' slow saves and loads.
#'
#' @return An (invisible) integer code, always 0.
#' @examples
#' clearUSGSProjectIndex()
#' @export
clearUSGSProjectIndex <- function() {
  USGSEnv$USGSLoadedProjectIndex <- ""
#  rm(USGSProjectsWebMerc, envir = USGSEnv)
  USGSEnv$USGSProjectsWebMerc <- ""
  invisible(0)
}

# ---------- USGSProjectIndexIsCached
#
#' USGS Lidar Toolkit -- Check For Project Index File Name and
#'   Boundary Cache
#'
#' Checks to see if there is a cached set of project polygons. Project
#' boundaries (index file name and polygons) are saved the first time
#' \code{queryUSGSProjectIndex} is called to speed up subsequent queries.
#' \code{USGSProjectIndexIsCached} lets you check to see if there is a
#' set of cached index polygons so you can better manage the \code{index}
#' parameter in calls to \code{queryUSGSProjectIndex}.
#'
#' @return An (invisible) integer code, always 0.
#' @examples
#' USGSProjectIndexIsCached()
#' @export
USGSProjectIndexIsCached <- function() {
  if ((USGSEnv$USGSLoadedProjectIndex != "") &&
      (grepl("Spatial", class(USGSEnv$USGSProjectsWebMerc), ignore.case = FALSE)))
    return(TRUE)

  return(FALSE)
}

# ---------- clearUSGSTileIndex
#
#' USGS Lidar Toolkit -- Clear the Stored Tile Index File Name
#'
#' Clear the name of the local lidar tile index file. The actual tile
#' polygons are only loaded for specific projects so they don't take
#' up as much memory as the project boundaries so the tile polygons
#' are not cached. Calling clearUSGSTileIndex() simply clears the
#' name of the local tile index file.
#'
#' @return An (invisible) integer code, always 0.
#' @examples
#' clearUSGSTileIndex()
#' @export
clearUSGSTileIndex <- function() {
  USGSEnv$USGSTileIndex <- ""
  #rm(USGSTilesWebMerc, envir = USGSEnv)
  #USGSEnv$USGSTilesWebMerc <- ""
  invisible(0)
}

# ---------- prepareTargetData
#
#' USGS Lidar Toolkit -- Prepare Target Data
#'
#' Prepare features for use when querying a lidar project or tile index to determine
#' available data coverage. The basic operation done by this function is to apply the
#' \code{buffer} to the features.
#'
#' @details Prepare point or polygon data for use when querying to determine the lidar
#'   project or tile coverage. Typically this preparation is done by the queryUSGSProjectIndex
#'   or queryUSGSTileIndex functions but you can use prepareTargetData to build the
#'   query objects so they can be used for other purposes.
#'
#'   Units for \code{buffer} are the same as the horizontal units for the input features
#'   so you shouldn't use data in LON-LAT with units of degrees.
#'
#' @param x Location or list of locations containing the easting for the center of
#'   the area-of-interest.
#' @param y Location or list of locations containing the northing for the center of
#'   the area-of-interest.
#' @param buffer Distance or list of distances added or subtracted to \code{(x,y)}
#'   to create the area-of-interest. Can be 0 when used with \code{(x,y)} to
#'   return projects providing coverage for point location(s). Can be a
#'   vector of values corresponding to the number of objects in \code{aoi}
#'   allowing a different buffer size for each object when \code{aoi} is a
#'   \code{Spatial*} of \code{sf} object. \code{buffer} can be negative with
#'   polygon features to reduce the area considered. However, you may end up
#'   with weird shapes if the distance is larger than the width of the polygon.
#' @param shape Character string describing the shape of the sample area.
#'   Valid values are \code{"square"} or \code{"circle"}. The buffer shape
#'   is the same for all objects specified by \code{(x,y)} or \code{aoi}.
#' @param aoi \code{Spatial*} or \code{sf} object containing a point(s) or polygon(s)
#'   describing the area(s) of interest. Can be points or polygons.
#' @param crs Valid \code{proj4string} string defining the coordinate
#'   reference system of \code{(x,y)} or \code{aoi}. \code{crs} is required when using
#'   \code{(x,y)}. It is optional when using \code{aoi}. This can also be a valid
#'   \code{SRS_string} for use with the sp::CRS() function where it will be provided
#'   as the \code{projargs} argument. \code{crs} can also be an EPSG code (numeric).
#' @param segments Number of segments to use when generating a circular
#'   area of interest. When using a \code{SpatialPoint*} or \code{sf} object
#'   with \code{shape = "circle"}, set \code{segments} to a rather large value (60
#'   or higher) that is a multiple of 4. The \code{gBuffer} function from
#'   \code{rgeos} and \code{st_buffer} function from \code{sf} are used to
#'   build the sample areas and it accepts the number of segments in a quarter
#'   circle so small values for \code{segments} may not produce good circles.
#'   Values for \code{segments} that are not a multiple of 4 will not
#'   produce circles with the correct number of segments.
#' @param returnType Character string specifying the object type for the
#'   polygon object returned by \code{prepareTargetData} when \code{(x,y)} is
#'   used to specify the area-of-interest. Valid values are \code{"Spatial"}
#'   or \code{"sf"}. \code{returnType} is ignored when \code{aoi} is specified
#'   and the return type will match the object type of \code{aoi}.
#' @return A set of optionally buffered spatial features. The return type will
#'   be the same as the \code{aoi} type. When \code{(x,y)} is used,
#'   \code{returnType} specifies the object type returned by \code{prepareTargetData}.
#' @examples
#' prepareTargetData(-13540901, 5806426, 180, shape = "circle",
#'   crs = sp::CRS(SRS_string="EPSG:3857")@projargs)
#' pt1 <- sf::st_point(c(-13540901, 5806426 + 500))
#' pt2 <- sf::st_point(c(-13540901 + 500, 5806426 - 500))
#' pt3 <- sf::st_point(c(-13540901 - 500, 5806426))
#' pt4 <- sf::st_point(c(-13540901 + 1000, 5806426 - 1000))
#' id <- c("P1", "P2", "P3", "P4")
#' x_sf <- sf::st_sf(data.frame(ID = id, stringsAsFactors = FALSE),
#'   geom = sf::st_sfc(pt1, pt2, pt3, pt4),
#'   crs = sp::CRS(SRS_string="EPSG:3857")@projargs)
#' samples <- prepareTargetData(aoi = x_sf, buffer = 180)
#' @export
prepareTargetData <- function(
  x,
  y,
  buffer = 0,
  shape = "square",
  aoi = "",
  crs = "",
  segments = 60,
  returnType = "sf"
) {
  if (inherits(aoi, "Spatial")) {
    # see if we have a Spatial* object
    if (grepl("Spatial", class(aoi), ignore.case = FALSE)) {
      if ((grepl("SpatialPolygons", class(aoi), ignore.case = FALSE)) ||
          (grepl("SpatialPoints", class(aoi), ignore.case = FALSE))) {
        if (length(buffer) > 1) {
          # check number of points and number of buffer values...should match
          if (length(buffer) != length(aoi)) {
            stop("When specify multiple buffer sizes, the number of buffers ",
              "must match the number of objects in aoi")
          }

          # specific buffer for each point
          if (tolower(shape) == "square") {
            target <- rgeos::gBuffer(aoi,
              byid = TRUE,
              width = buffer,
              capStyle = "SQUARE"
            )
          } else {
            # circular buffer
            target <- rgeos::gBuffer(aoi,
              byid = TRUE,
              width = buffer,
              quadsegs = ceiling(segments / 4),
              capStyle = "ROUND"
            )
          }
        } else {
          if (buffer == 0.0) {
            # use points without buffers as-is
            target = aoi
          } else {
            # add square buffers around points
            # check for different buffer sizes...vector of sizes
            # buffers correspond to each point
            if (tolower(shape) == "square") {
              target <- rgeos::gBuffer(aoi,
                byid = TRUE,
                width = rep(buffer, length(aoi)),
                capStyle = "SQUARE"
              )
            } else {
              # add circular buffers around points
              target <- rgeos::gBuffer(aoi,
                byid = TRUE,
                width = rep(buffer, length(aoi)),
                quadsegs = ceiling(segments / 4),
                capStyle = "ROUND"
              )
            }
          }
        }
      } else {
        stop("unsupported Spatial* object:",
          class(aoi))
      }
      # if (length(target) > 1)
      #   message("aoi contains multiple features. Individual features ",
      #     "will not be associated with individual lidar project ",
      #     "polygons.")
    } else {
      stop("unsupported class for aoi")
    }
  } else if (any(grepl("sf", class(aoi), ignore.case = FALSE))) {
    # have sf-derived object...use sf functions and return sf object
    if ((all(sf::st_dimension(aoi) == 2)) || (all(sf::st_dimension(aoi) == 0))) {
      # points
      if (length(buffer) > 1) {
        # check number of points and number of buffer values...should match
        if (length(buffer) != length(sf::st_geometry(aoi))) {
          stop("When specify multiple buffer sizes, the number of buffers ",
            "must match the number of objects in aoi")
        }

        # specific buffer for each point
        if (tolower(shape) == "square") {
          target <- sf::st_buffer(aoi,
            dist = buffer,
            nQuadSegs = 5,
            endCapStyle = "SQUARE"
          )
        } else {
          # circular buffer
          target <- sf::st_buffer(aoi,
            dist = buffer,
            nQuadSegs = ceiling(segments / 4),
            endCapStyle = "ROUND"
          )
        }
      } else {
        if (buffer == 0.0) {
          # use points without buffers as-is
          target = aoi
        } else {
          # add square buffers around points
          # check for different buffer sizes...vector of sizes
          # same buffer applied to each point
          if (tolower(shape) == "square") {
            target <- sf::st_buffer(aoi,
              dist = rep(buffer, length(sf::st_geometry(aoi))),
              nQuadSegs = 5,
              endCapStyle = "SQUARE"
            )
          } else {
            # add circular buffers around points
            target <- sf::st_buffer(aoi,
              dist = rep(buffer, length(sf::st_geometry(aoi))),
              nQuadSegs = ceiling(segments / 4),
              endCapStyle = "ROUND"
            )
          }
        }
      }
    } else {
      stop("sf* object for aoi must be points or polygons")
    }
  } else {
    # create a sf object using (x,y), buffer, shape and crs

    # make sure we have crs
    if (crs == "")
      stop("no crs provided for (x,y)")

    # check returnType
    if (tolower(returnType) == "spatial") {
      # make a point feature
      tempTarget <- sp::SpatialPointsDataFrame((rbind(c(x, y))),
        data.frame("ID" = c("P1"), stringsAsFactors = FALSE),
        proj4string = sp::CRS(sf::st_crs(crs)$proj4string))

      if (length(buffer) > 1) {
        # check number of points and number of buffer values...should match
        if (length(buffer) != length(tempTarget)) {
          stop("When specify multiple buffer sizes, the number of buffers ",
            "must match the number of objects in aoi")
        }

        # specific buffer for each point
        if (tolower(shape) == "square") {
          target <- rgeos::gBuffer(tempTarget,
            byid = TRUE,
            width = buffer,
            capStyle = "SQUARE"
          )
        } else {
          # circular buffer
          target <- rgeos::gBuffer(tempTarget,
            byid = TRUE,
            width = buffer,
            quadsegs = ceiling(segments / 4),
            capStyle = "ROUND"
          )
        }
      } else {
        if (buffer == 0.0) {
          # use point as-is
          target = tempTarget
        } else {
          # buffer the point
          # buffer is same for each point
          if (tolower(shape) == "square") {
            target <- rgeos::gBuffer(tempTarget,
              byid = TRUE,
              width = rep(buffer, length(tempTarget)),
              capStyle = "SQUARE"
            )
          } else {
            # add circular buffers around points
            target <- rgeos::gBuffer(tempTarget,
              byid = TRUE,
              width = rep(buffer, length(tempTarget)),
              quadsegs = ceiling(segments / 4),
              capStyle = "ROUND"
            )
          }
        }
      }
    } else if (tolower(returnType) == "sf") {
      tempTarget <- sf::st_sf(data.frame(ID = "P1", stringsAsFactors = F), geom = sf::st_sfc(sf::st_point(c(x, y))), crs = crs)

      if (buffer == 0.0) {
        target <- tempTarget
      } else {
        # buffer the point
        # add square buffers around points
        if (tolower(shape) == "square") {
          target <- sf::st_buffer(tempTarget,
            dist = buffer,
            nQuadSegs = 5,
            endCapStyle = "SQUARE"
          )
        } else {
          # add circular buffers around points
          target <- sf::st_buffer(tempTarget,
            dist = buffer,
            nQuadSegs = ceiling(segments / 4),
            endCapStyle = "ROUND"
          )
        }

        # creates a square or round buffer "manually"
        # if (tolower(shape) == "square") {
        #   xc <- c(x - buffer, x + buffer, x + buffer, x - buffer, x - buffer)
        #   yc <- c(y - buffer, y - buffer, y + buffer, y + buffer, y - buffer)
        #   target <- sf::st_sf(data.frame(ID = "S1", stringsAsFactors = F),
        #     geom = sf::st_sfc(sf::st_polygon(list(cbind(xc, yc)))),
        #     crs = crs)
        # } else {
        #   # circle code taken from https://rdrr.io/cran/sampSurf/src/R/spCircle.R
        #   # left half of the circle, then right...
        #   circ = seq(0, 2*pi, len = segments + 1)
        #
        #   #   make the circle outline...
        #   circle = matrix(c(x + buffer * cos(circ),
        #     y + buffer * sin(circ),
        #     rep(1, segments + 1) ),
        #     nrow = segments + 1)
        #
        #   # any little difference between start & end pts with identical()
        #   # can mess up the the sp package Polygon routine, so set the end point
        #   # exactly to start, then transform...
        #   circle = rbind(circle, circle[1,])
        #   dimnames(circle) = list(NULL,c('x','y','hc'))
        #
        #   # make a sf polygon object...
        #   target <- sf::st_sf(data.frame(ID = "C1", stringsAsFactors = F),
        #     geom = sf::st_sfc(sf::st_polygon(list(circle[, -3]))),
        #     crs = crs)
        # }
      }
    } else {
      stop("invalid return type: ", returnType)
    }
  }
  return(target)
}

# ---------- generatePolygonSamplePoints
#
#' USGS Lidar Toolkit -- Create sample points for a set of polygon
#'
#' Generate a set of sample points within a polygon. Given an input polygon,
#' create a set of sample points within the polygon. Multi-part polygons are allowed
#' and the number of sample points will be distributed over the parts.
#'
#' There are 3 potential interactions between \code{gridSpacing} and \code{count}:
#'   Both have values > 0: \code{gridSpacing} controls the sample point spacing. If the number of
#'   sample points exceeds \code{count}, the set of points is sampled to produce \code{count} points.
#'   \code{count} > 0; \code{gridSpacing} = -1: point spacing is computed using \code{count} and the polygon
#'   area.
#'   \code{count} = -1; \code{gridSpacing} > 0: \code{gridSpacing} controls the sample point spacing and
#'   you get all sample points.
#'
#'   The number of sample points may not exactly match \code{count} due to irregularities in the
#'   the polygon. If \code{count} is between 0 and 1, it is interpreted as the proportion of the
#'   sample points that should be returned. For example, if you want a 10 percent sample of the points
#'   located on a 100 meter grid, set \code{count = 0.1} and \code{gridSpacing = 100}.
#'
#'   If you perturb the locations without using a \code{polyBuffer}, you can end up with points outside
#'   the polygon. The final set of sample points is intersected with the original polygon
#'   so the points outside the polygon are eliminated. However, this final clipping operation
#'   can mess up the \code{count} (number of sample points). This may not be a problem
#'   when \code{gridSpacing} and \code{count} have values (not = -1) because the sampling to reduce the number
#'   of points (if needed) occurs after the clipping operation.
#'
#'   One caution with \code{polyBuffer}:
#'   If you specify a value for \code{polyBuffer} larger than the width of a polygon, you can essentially remove the
#'   polygon so you end up with no sample points. This is especially a problem when you have
#'   project polygons made up of several parts and some of the parts are small. Units for the buffer are
#'   the same as the horizontal units for the input feature so you shouldn't use data in LON-LAT with units
#'   of degrees.
#'
#'   You can call this function with multiple polygons to generate a set of sample points for
#'   each polygon. Each set will have \code{count} points (at least the logic tries to generate \code{count} points).
#'
#' @param inputPolygons \code{Spatial*} or \code{sf} object containing the polygon(s) for
#'   which you want sample points.
#' @param IDColumnLabel Character: Name of the column in the \code{inputPolygons} data to
#'   be used to build the identifier for the sample points. If not specified, identifiers
#'   for the sample points are simple sequential numbers starting at 1. The same sequence
#'   of identifiers is generated for all polygons so you should set \code{appendAttributes}
#'   to TRUE to ensure you have enough information to uniquely identify each sample point when
#'   not specifying \code{IDColumnLabel}.
#' @param appendAttributes Logical: if TRUE, the fields for the \code{inputPolygons} are appended
#'   to the fields of the returned point data. If FALSE, only a label field is added to
#'   the fields of the return points. The label field is named \code{Label} unless
#'   \code{columnNameID} is specified.
#' @param columnNameID Character: Name for the identifier field in the set of sample points.
#'   If not specified, the name will be \code{"Label"}.
#' @param pattern Character indicating the desired sample pattern. \code{pattern} will be
#'   passed to \code{spsample()}. Choices are: "random" for completely spatial random; "regular"
#'   for regular (systematically aligned) sampling; and "hexagonal" for sampling on a hexagonal
#'   lattice. While other options are available in (and will be passed to) \code{spsample()},
#'   their behavior as used in this application is unknown.
#' @param polyBuffer Distance to reduce the size of the polygon(s) by applying \code{rgeos::gbuffer}
#'   with \code{width = -polyBuffer}. A value <= 0 will produce no buffer.
#' @param gridSpacing Desired grid size passed to \code{spsample()}.
#' @param offset Position of the regular sample grid in the unit cell [0,1] x [0,1]. The default
#'   is \code{c(0.5,0.5)} placing the position of the grid at the center of the sample cell.
#'   When \code{pattern = "regular"}, the effect of \code{offset} is easy to interpret.
#'   However, when \code{pattern = "hexagonal"} or \code{pattern = "random"}, the effect
#'   of \code{offset} is much harder to interpret.
#' @param count Desired number of sample points. You may not actually get back \code{count}
#'   points depending on the interaction between the polygon shape, \code{buffer} and
#'   \code{count)}. If between 0 and 1, \code{count} is interpreted as a proportion of
#'   the number of sample points generated that should be returned. This is useful when
#'   generating sample points for polygons that vary in size.
#' @param perturb Logical: If TRUE, point locations are randomly moved from the location
#'   generated by \code{spsample()}. The distance moved is controlled by
#'   \code{perturbMultiplier * gridSpacing}.
#' @param perturbMultiplier Multiplier > 0 used to adjust locations generated
#'   by \code{spsample()}.
#' @param minArea Minimum area for polygons used to create sample points. This is the total
#'   area covered by the feature so it might include several polygons. The area is evaluated
#'   before and after the \code{buffer} is applied. If -1 (default), the minimum area is
#'   computed as \code{gridSpacing^2 * 5}. If 0, sampling will be attempted for all features.
#'   However, sample generation may fail for small features depending on the values for
#'   \code{buffer} and \code{gridSpacing}.
#' @param seedColumnLabel Character: Name of the column in \code{inputPolygons} containing
#'   an integer value used to initialize the random number generator. Using a field
#'   unique to a polygon allows you to generate a consistent sample every time a particular
#'   polygon is used. When \code{seedColumnLabel} is not specified, a single seed value
#'   (if provided in \code{seed}) is used for all polygons. If a seed value is not provided
#'   in \code{seed}, the random number generator is not seeded.
#' @param seed Seed for the random number generator to be applied before any sample
#'   generation (outside the loop used to generate sample locations for each polygon in
#'   \code{inputPolygons}).
#' @param verbose if > 0, output status information every \code{verbose} features
#'   in \code{inputPolygons}.
#' @return A set of sample point locations attributed with X, Y, and an identifier created
#'   using either \code{IDColumnLabel} and the sequential point number (when \code{IDColumnLabel}
#'   is specified) or just the sequential point number (when \code{IDColumnLabel} is not
#'   specified) and all columns from the \code{inputPolygons} data frame. The return type
#'   will be determined by the type of \code{inputPolygons}. If \code{inputPolygons} is
#'   a \code{Spatial*} type, the return will be a \code{SpatialPointsDataFrame} object and
#'   if \code{inputPolygons} is a simple features \code{(sf)} type, the return will be a
#'   simple features \code{(sf)} type.
#' @export
generatePolygonSamplePoints <- function(
  inputPolygons,
  IDColumnLabel = "",
  appendAttributes = TRUE,
  columnNameID = "",
  pattern = "hexagonal",
  polyBuffer = -1,
  gridSpacing = 5000.0,
  offset = c(0.5, 0.5),
  count = -1,
  perturb = TRUE,
  perturbMultiplier = 1.0,
  minArea = -1,
  seedColumnLabel = "",
  seed = -1,
  verbose = 0
) {
  # -------------------------------------------------------------------------------------------------
  #                        Create a set of sample locations for each feature
  # -------------------------------------------------------------------------------------------------
  #
  # Idea is to overlay the polygon(s) with a set of sample points using a desired spacing.
  # The spacing seems to drive the number of points. If you include a count and the spacing
  # results in more than count points, the set of points will be sampled to produce the target
  # number of points.
  #
  # good sampling reference:
  # https://rstudio-pubs-static.s3.amazonaws.com/200263_e77d00d6b6b24aa8890c8c4f074bcdff.html

  polys <- inputPolygons

  # check the input type
  convertTosf <- FALSE
  if (!inherits(polys, "Spatial")) {
    polys <- sf::as_Spatial(inputPolygons)
    convertTosf <- TRUE
  }

  # sort out the minimum area
  minimumArea <- minArea
  if (minimumArea < 0)
    minimumArea <- gridSpacing * gridSpacing * 5

  if (count == -1 && gridSpacing == -1)
    stop("You must specify either count or gridSpacing")

  if (seedColumnLabel == "" && seed > 0)
    set.seed(seed)

  firstPoly <- TRUE
  for (thePoly in 1:nrow(polys@data)) {
    if (polys@polygons[[thePoly]]@area >= minimumArea) {
      currentPoly <- polys[thePoly, ]

      # seed the random number generator
      # goal is to be able to reproduce locations for each polygon
      if (seedColumnLabel != "")
        set.seed(polys@data[thePoly, seedColumnLabel])

      # buffer the polygon to the inside
      if (polyBuffer > 0) {
        currentPoly <- rgeos::gBuffer(polys[thePoly, ],
                                      byid = TRUE,
                                      width = -polyBuffer)
      }

      if (currentPoly@polygons[[1]]@area >= minimumArea) {
        # generate sample points...full grid over target polygon
        if (count <= 1) {
          dummyLocations <- sp::spsample(currentPoly,
                                         type = pattern,
                                         offset = offset,
                                         cellsize = gridSpacing)
        } else if (gridSpacing == -1) {
          dummyLocations <- sp::spsample(currentPoly,
                                         type = pattern,
                                         offset = offset,
                                         n = count)
        } else {
          dummyLocations <- sp::spsample(currentPoly,
                                         type = pattern,
                                         offset = offset,
                                         cellsize = gridSpacing,
                                         n = count)
        }

        # perturb the X and Y values using perturbMultiplier...this will let you go from uniform
        # with some variation to uniform random.
        if (perturb) {
          for (pt in 1:nrow(dummyLocations@coords)) {
            dummyLocations@coords[pt, 1] <- dummyLocations@coords[pt, 1] + (stats::runif(1, -0.5, 0.5) * gridSpacing * perturbMultiplier)
            dummyLocations@coords[pt, 2] <- dummyLocations@coords[pt, 2] + (stats::runif(1, -0.5, 0.5) * gridSpacing * perturbMultiplier)
          }
        }

        # clip the points to the original polygon
        dummyLocations <- dummyLocations[currentPoly, ]

        # sort out the count
        targetCount <- count
        if (targetCount > 0 && targetCount <= 1)
          targetCount <- max(nrow(dummyLocations@coords) * count, 1)

        # draw a sample of points if we have more points than requested
        if (targetCount > 0 && nrow(dummyLocations@coords) > targetCount) {
          dummyList <- raster::sampleInt(nrow(dummyLocations@coords), targetCount, replace = FALSE)

          # generate list of dummy locations to use
          dummyLocations <- dummyLocations[dummyList, ]

          rm(dummyList)
        }

        # update bounding box
        dummyLocations@bbox <- sp::bbox(dummyLocations@coords)

        # add location identifier
        pt_id = seq(1, nrow(dummyLocations@coords))

        # add columns to spatial points...(XY) and a label
        if (IDColumnLabel == "") {
          data <- data.frame(Label = pt_id,
                             row.names = NULL,
                             stringsAsFactors = FALSE)
        } else {
          data <- data.frame(Label = paste(polys@data[thePoly,  IDColumnLabel], pt_id, sep = "_"),
                             row.names = NULL,
                             stringsAsFactors = FALSE)
        }

        # data <- data.frame(X = dummyLocations@coords[, 1],
        #                    Y = dummyLocations@coords[, 2],
        #                    Label = paste(polys@data[thePoly,  IDColumnLabel], pt_id, sep = "_"),
        #                    row.names = NULL,
        #                    stringsAsFactors = FALSE)

        if (columnNameID != "") {
          colnames(data) <- c(columnNameID)
        }

        # append all columns from the polygon
        if (appendAttributes) {
          data <- cbind(data, polys@data[thePoly, ], row.names = NULL)
        }

        # add data to SpatialPoints object...accumulate if more than 1 feature (polygon)
        if (thePoly == 1) {
          dummyPlots <- sp::SpatialPointsDataFrame(dummyLocations, data)
          firstPoly <- FALSE
        } else {
          dummyPlots <- rbind(dummyPlots, sp::SpatialPointsDataFrame(dummyLocations, data))
        }

        if (verbose > 0) {
          if ((thePoly %% verbose) == 0)
            message(paste0("Generated sample for polygon set: ", thePoly))
        }
      }
    }
  }

  if (nrow(dummyPlots) && convertTosf)
    dummyPlots <- sf::st_as_sf(dummyPlots)

  if (nrow(dummyPlots))
    return(dummyPlots)
  else
    return(NULL)
}

# ---------- buildPDALPipelineENTWINE
#
#' USGS Lidar Toolkit -- Create a PDAL pipeline to retrieve data from the ENTWINE collection
#'
#' Generate PDAL pipeline(s) to retrieve point data for a set of polygons. Given the input polygon(s),
#' create PDAL pipeline(s) to retrieve data for each polygon. Input data must be polygons and must have
#' a field that contains the URL for the ept.json file associated with the ENTWINE data set.
#' Input polygons can be either an \code{sp} or \code{sf} feature set.
#'
#' @details The pipeline template must include at least the following items for the ept_reader: bounds
#'   and placeholder, and for the las_writer: filename. There is a template provided with the
#'   library that retrieves all data within the bounding box for a feature and stores is in compressed
#'   (LAZ) format. Users can supply their own template (in json format) but only the values
#'   mentioned for the ept_reader and las_writer tags will be modified.
#'
#'   The pipeline template provided with the USGSlidar package is stored in the install folder
#'   for the package under the \code{extdata} folder. While you can directly modify this copy
#'   of the file, it would be better to copy the file to another location, make the desired
#'   changes and then specify your new template using the \code{pipelineTemplateFile} parameter.
#'
#'   The file name for the data clips is formed using the basename of the value in the
#'   \code{URLColumnLabel} field and the value in the \code{IDColumnLabel} separated
#'   by "_". This helps ensure that clips can be differentiated when locations are covered
#'   by more than one lidar project.
#'
#' @param inputPolygons \code{Spatial*} or \code{sf} object containing a polygon(s) for
#'   which you want to retrieve data. The polygons must include a field named \code{url}
#'   that contains the URL for the ENTWINE data (ept.json) containing the polygon or the
#'   name of the column containing the URL must be specified in \code{URLColumnLabel}.
#' @param IDColumnLabel Character: Name of the column in \code{inputPolygons} to be used to build
#'   the output file name for the retrieved point data.
#' @param URLColumnLabel Character: Name of the column containing the URL for the ENTWINE data.
#'   Default is "url". For Entwine data, this is typically the URL for the ett.json file.
#' @param pipelineOutputFileBaseName Character: base name for the new PDAL pipeline. This is
#'   used along with the value in \code{IDColumnLabel} to create the actual file name. If not
#'   specified, the pipeline file name will be created using the base name of the value
#'   in the \code{url} field and the value in \code{IDColumnLabel}. \code{pipelineOutputFileBaseName}
#'   should not contain any path information.
#' @param pipelineOutputFolder Character: full path name for the new PDAL pipeline(s). This should
#'   be a fully qualified path. Pipelines will be saved in this folder. If not specified,
#'   pipelines will be saved in the current working folder. This folder will be created
#'   if it does not exist.
#' @param pipelineTemplateFile Character: full file specifier for the JSON file used
#'   as a template for the output PDAL pipeline. See the description for the required items in
#'   the template. A simple default template will be used if \code{pipelineTemplateFile}
#'   is not provided.
#' @param clipOutputFolder Character: full path name for the point data file(s). This should
#'   be a fully qualified path. Point files will be saved in this folder. If not specified,
#'   point files will be saved in the current working folder. This folder will be created
#'   if it does not exist. the file name for the point data file(s) is created using the
#'   value in the \code{url} and \code{IDColumnLabel} fields. The file name extension will be
#'   ".las" if \code{compress = FALSE} or ".laz" if \code{compress = TRUE} or \code{compress}
#'   is not specified.
#' @param pipelineScript Character: file name for the script to run the pipelines. For Windows
#'   systems, this will be a batch file. No other operating systems are supported. The script
#'   will be written in the folder defined by \code{pipelineOutputFolder}.
#' @param compress Logical: If TRUE (default), point data area stored in LAZ format; if FALSE,
#'   point data are stored in LAS format.
#' @param verbose if > 0, output status information every \code{verbose} features
#'   in \code{inputPolygons}.
#' @return Integer: (invisible) number of pipeline files created.
#' @examples
#' \dontrun{
#' buildPDALPipelineENTWINE(plot_polys)
#' }
#' @export
buildPDALPipelineENTWINE <- function(
  inputPolygons,
  IDColumnLabel,
  URLColumnLabel = "url",
  pipelineOutputFileBaseName = "",
  pipelineOutputFolder = ".",
  pipelineTemplateFile = "",
  clipOutputFolder = ".",
  pipelineScript = "RUNME.bat",
  compress = TRUE,
  verbose = 0
) {
  # -------------------------------------------------------------------------------------------------
  # Create a PDAL pipeline to retrieve data from the ENTWINE collection
  # -------------------------------------------------------------------------------------------------
  #
  # Idea is to modify an exiting pipeline file to populate the ept_reader:bounds,
  # ept_reader:filename, las_writer:compression, and las_writer:filename tags with
  # information specific to the input polygons. The user can provide their own pipeline
  # template to perform additional processing of the data.

  polys <- inputPolygons

  # check the input type...we want to work only with Spatial objects in the function
  if (!inherits(polys, "Spatial")) {
    polys <- sf::as_Spatial(inputPolygons)
  }

  # make sure we have features
  if (nrow(polys@data) == 0)
    return(0)

  # check output folders...if provided
  if (pipelineOutputFolder != ".") {
    if (!dir.exists(pipelineOutputFolder))
      dir.create(pipelineOutputFolder, recursive = TRUE)

    pipelineOutputFolder <- normalizePath(pipelineOutputFolder)
  }

  if (clipOutputFolder != ".") {
    if (!dir.exists(clipOutputFolder))
      dir.create(clipOutputFolder, recursive = TRUE)

    clipOutputFolder <- normalizePath(clipOutputFolder)
  }

  # deal with pipeline template...read the default if a template isn't provided
  pipelineTemplatelocal <- NULL
  if (pipelineTemplateFile == "") {
    # read template from package data
    pipelineTemplatelocal <- jsonlite::fromJSON(system.file("extdata", "plots.json", package = "USGSlidar", mustWork = TRUE))
  } else {
    # read user provided template
    pipelineTemplatelocal <- jsonlite::fromJSON(pipelineTemplateFile)
  }

  # check the template
  if (nrow(pipelineTemplatelocal) == 0) {
    # bad template
    stop("Bad template file: ", pipelineTemplateFile)
  }

  # set extension for output point file
  if (compress) {
    pointExtension <- ".laz"
  } else {
    pointExtension <- ".las"
  }

  # write first line of the processing script...comment statement that can be used to install
  # PDAL into the anaconda environment
  write("rem conda install -c conda-forge pdal -y",
        file = paste(pipelineOutputFolder, "\\", pipelineScript, sep = ""))

  sampleCount <- nrow(polys@data)

  # step through features and build pipelines
  for (thePoly in 1:sampleCount) {
    # display verbose progress
    if (verbose > 0) {
      if ((thePoly %% verbose) == 0)
        message(paste0("Sample ", thePoly, " of ", sampleCount))
    }

    # get feature bounding box
    bb <- sf::st_bbox(polys[thePoly,])

    # make substitutions
    # tag:ept_reader url, bounding box, resolution, #threads
    pipelineTemplatelocal[pipelineTemplatelocal$tag == "ept_reader", "filename"] <- polys@data[thePoly, URLColumnLabel]
    pipelineTemplatelocal[pipelineTemplatelocal$tag == "ept_reader", "bounds"] <- paste("([", bb[1], ",", bb[3], "],[", bb[2], ",", bb[4], "])")
    pipelineTemplatelocal[pipelineTemplatelocal$tag == "ept_reader", "resolution"] <- "0.01"
    pipelineTemplatelocal[pipelineTemplatelocal$tag == "ept_reader", "threads"] <- "4"

    # tag:las_writer file name and compression
    lasFile <- paste(clipOutputFolder, "\\",
                     basename(dirname(polys@data[thePoly, URLColumnLabel])),
                     "_",
                     polys@data[thePoly, IDColumnLabel],
                     pointExtension,
                     sep = "")
    pipelineTemplatelocal[pipelineTemplatelocal$tag == "las_writer", "filename"] <- lasFile

    if (compress) {
      pipelineTemplatelocal[pipelineTemplatelocal$tag == "las_writer", "compression"] <- "laszip"
    } else {
      pipelineTemplatelocal[pipelineTemplatelocal$tag == "las_writer", "compression"] <- "false"
    }

    # write pipeline file
    if (pipelineOutputFileBaseName != "") {
      jsonFile <- paste(pipelineOutputFolder, "\\",
                        pipelineOutputFileBaseName,
                        "_",
                        polys@data[thePoly, IDColumnLabel],
                        ".json",
                        sep = "")
    } else {
      jsonFile <- paste(pipelineOutputFolder, "\\",
                        basename(dirname(polys@data[thePoly, URLColumnLabel])),
                        "_",
                        polys@data[thePoly, IDColumnLabel],
                        ".json",
                        sep = "")
    }
    write(jsonlite::toJSON(pipelineTemplatelocal, pretty = TRUE), file = jsonFile)

    # write command to run pipeline to batch file...enclose file name in quotes
    write(paste("pdal pipeline ",
                "\"",
                jsonFile,
                "\"",
                sep = ""),
          file = paste(pipelineOutputFolder, "\\", pipelineScript, sep = ""),
          append = TRUE)
  }

  # read the batch file used to run the pipelines and randomize the order of the calls.
  # Keep the first line in place (comment to install PDAL)
  # This will mix the dummy locations and valid locations together to help obfuscate the
  # valid plot locations.
  commands <- utils::read.table(paste(pipelineOutputFolder, "\\", pipelineScript, sep = ""),
                         sep = "\n",
                         stringsAsFactors = FALSE)

  if (nrow(commands) > 1) {
    commands$rnum <- stats::runif(nrow(commands))
    commands[1, "rnum"] <- 0
    commands <- commands[order(commands$rnum),]

    # write the commands back to the batch file
    write(commands[, "V1"], paste(pipelineOutputFolder, "\\", pipelineScript, sep = ""), sep = "\r\n")
  }

  invisible(nrow(commands) - 1)
}

#
# -----------------------------------------------------------------------------
# End of library code
# -----------------------------------------------------------------------------
#
