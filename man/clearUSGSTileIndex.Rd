% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoreFunctions.R
\name{clearUSGSTileIndex}
\alias{clearUSGSTileIndex}
\title{USGS Lidar Toolkit -- Clear the Stored Tile Index File Name}
\usage{
clearUSGSTileIndex()
}
\value{
An (invisible) integer code, always 0.
}
\description{
Clear the name of the local lidar tile index file. The actual tile
polygons are only loaded for specific projects so they don't take
up as much memory as the project boundaries so the tile polygons
are not cached. Calling clearUSGSTileIndex() simply clears the
name of the local tile index file.
}
\examples{
clearUSGSTileIndex()
}
