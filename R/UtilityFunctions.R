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
#' defined in the web Mercator projection to their size when reprojected
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

    buf <- desiredBuffer * (1.0 + ((-0.3065 + 0.3780 * sf::st_coordinates(t)[, 2])^2 / 1000))
  }
  return(buf)
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

