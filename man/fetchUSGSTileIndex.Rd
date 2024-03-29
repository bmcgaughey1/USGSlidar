% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FetchFunctions.R
\name{fetchUSGSTileIndex}
\alias{fetchUSGSTileIndex}
\title{USGS Lidar Toolkit -- Download Lidar Tile Index}
\usage{
fetchUSGSTileIndex(destfile = "", url = "", method = "libcurl", ...)
}
\arguments{
\item{destfile}{A character string specifying the file name for the downloaded
 tile index. If not provided or empty, the index is stored in the current
working directory using the filename in the url.}

\item{url}{A character string with the URL for tile index file. If an empty
string, FESM_LPC_TILE.gpkg from the USGS rockyweb server is used.}

\item{method}{Method used with \code{download.file()}}

\item{...}{Additional arguments passed to \code{download.file()}}
}
\value{
An (invisible) integer code, 0 for success and non-zero for failure.
  This is the value returned from \code{download.file}.
}
\description{
Downloads the USGS tile index file from the USGS rockyweb server. The tile
index is the LPC_TESM geopackage that is updated by the USGS as new lidar
data are added to the published collection of data. This file is large so it
usually makes sense to download it once and keep a local copy. However, the
files also changes daily so if you want the latest index, you should download
the index more frequently.
}
\details{
When retrieving large files, it may be necessary to increase the timeout period so
downloads can complete. This is done using the following line of code (sets timeout
to 5000 seconds):

\code{options(timeout = max(5000, getOption("timeout")))}

You can also download the tile index manually and then use \code{setUSGSTileIndex()}
  to activate the index for use with \code{USGSlidar} functions.
}
\examples{
\dontrun{
fetchUSGSTileIndex("")
fetchUSGSTileIndex("Tiles.gpkg")
fetchUSGSTileIndex("Tiles.gpkg", method = "wininet")
}
}
