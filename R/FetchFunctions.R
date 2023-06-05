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
#' When retrieving large files, it may be necessary to increase the timeout period so
#' downloads can complete. This is done using the following line of code (sets timeout
#' to 5000 seconds):
#'
#' \code{options(timeout = max(5000, getOption("timeout")))}
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
#' When retrieving large files, it may be necessary to increase the timeout period so
#' downloads can complete. This is done using the following line of code (sets timeout
#' to 5000 seconds):
#'
#' \code{options(timeout = max(5000, getOption("timeout")))}
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

# ---------- fetchUSGSTiles
#
#' USGS Lidar Toolkit -- Download Lidar Tiles
#'
#' Download one or more files (usually lidar data tiles) described
#' by a list of URLs and place them in a folder.
#'
#' When retrieving large files, it may be necessary to increase the timeout period so
#' downloads can complete. This is done using the following line of code (sets timeout
#' to 5000 seconds):
#'
#' \code{options(timeout = max(5000, getOption("timeout")))}
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

