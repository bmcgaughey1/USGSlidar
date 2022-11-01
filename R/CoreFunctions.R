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
#' @param url A character string with the URL for the file. If using local files,
#'   the URL should start with \code{file://} and you should set \code{method="auto"}.
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

