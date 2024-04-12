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
#' @param polyBuffer Distance to reduce the size of the polygon(s) by applying \code{sf::st_buffer}
#'   with \code{dist = -polyBuffer}. A value <= 0 will produce no buffer.
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
        currentPoly <- sf::as_Spatial(
          sf::st_buffer(sf::st_as_sf(polys[thePoly, ]), dist = -polyBuffer, singleSide = T)
        )
        # currentPoly <- rgeos::gBuffer(polys[thePoly, ],
        #                               byid = TRUE,
        #                               width = -polyBuffer)
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
