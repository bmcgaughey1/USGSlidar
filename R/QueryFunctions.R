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
    emptyCount <- sum(is.na(sf::st_dimension(projectsWebMerc)))
    if (emptyCount)
      message("Found", emptyCount, "features with empty geometries")

    # corrupt
    corruptCount <- sum(is.na(sf::st_is_valid(projectsWebMerc)))
    if (corruptCount)
      message("Found", corruptCount, "features with corrupt geometries")

    # invalid
    # na.omit is causing problems...not sure what the original intent was for using na.omit so I removed it
    # also not sure what package na.omit was coming from. Could be terra or stats
    #invalidCount <- sum(any(na.omit(sf::st_is_valid(projectsWebMerc)) == FALSE))
    invalidCount <- sum(any(sf::st_is_valid(projectsWebMerc)) == FALSE)
    if (invalidCount)
      message("Found ", invalidCount, " features with invalid geometries")

    if (emptyCount + corruptCount + invalidCount) {
      if (emptyCount) {
        stop(paste("Index contains features with empty geometries",
                   "that cannot be fixed...aborting"))
        return(NULL)
      } else {
        if (verbose) message("--Attempting to clean index polygons using st_make_valid")
        projectsWebMerc <- sf::st_make_valid(projectsWebMerc)

        # na.omit is causing problems...not sure what the original intent was for using na.omit so I removed it
        #if (any(stats::na.omit(sf::st_is_valid(projectsWebMerc) == FALSE))) {
        if (any(sf::st_is_valid(projectsWebMerc) == FALSE)) {
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
