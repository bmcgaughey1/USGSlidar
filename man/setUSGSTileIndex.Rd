% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoreFunctions.R
\name{setUSGSTileIndex}
\alias{setUSGSTileIndex}
\title{USGS Lidar Toolkit -- Set Filename for the Lidar Tile Index}
\usage{
setUSGSTileIndex(index)
}
\arguments{
\item{index}{Character string with the file name for the tile index.}
}
\value{
An (invisible) integer code, 0 for success and non-zero for failure.
}
\description{
Set the filename and path for the local copy of the USGS lidar tile
index file. This is used when you have already used fetchUSGSTileindex
to download a tile index. The tile index is quite large and it is
frequently updated by the USGS. However, it may not be necessary to
download a new copy every time you want to download lidar data.
}
\examples{
\dontrun{
setUSGSTileIndex("g:/working_data/TileIndex.gpkg")
}
}
