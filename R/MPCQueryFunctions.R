# ---------- queryMPCTileIndex
#
#' USGS Lidar Toolkit -- Identify Microsoft Planetary Computer Lidar Tiles Covering Area
#'
#' Intersect a set of features (points or polygons) against the lidar point data STAC index
#' on Microsoft Planetary Computer to determine which tiles are needed to provide coverage for the
#' features. This index contains every lidar point tile in the MPC-USGS lidar
#' collection. Queries for features covering a large area can take some time.
#'
#' The MPC lidar collection contains all lidar data produced by USGS as of Fall 2022.
#' Projects added to the USGS data collection after this date are currently not added
#' to the MPC data collection. This could change in the future. Check MPC for additional
#' information about the lidar data colection.
#'
#' @details Query the MPC tile index to find tiles that intersect the \code{buffer}ed
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
#'   Data in \code{(x,y)} and \code{aoi} are projected into the UTM
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
#' @param segments Number of segments to use when generating a circular
#'   area of interest. When using a \code{SpatialPoint*} or \code{sf} object
#'   with \code{shape = "circle"}, set \code{segments} to a rather large value (60
#'   or higher) that is a multiple of 4. The \code{st_buffer} function from \code{sf} is used to
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
#'   information included with the project shape attributes. There are two
#'   types of URLs returned for the lidar tiles. The \code{URL} field contains
#'   the unsigned URL for the tiles and the \code{signedURL} field contains a signed URL.
#'   The signed URLs are only valid for the current R session and cannot be
#'   saved and used at a later time. If you need to use the URLs in a different R
#'   session or a different process, you will need to obtain access tokens and
#'   append them to the URLs. If you are using R, you can use the \code{signMPCURL}
#'   function in the \code{USGSlidar} package to sign the URLs.
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
#' @param useLegacyBuffering Boolean flag indicating that the \code{buffer} should
#'   be applied to features in their original projection. This was the original
#'   behavior of \code{prepareTargetData} prior to changes in June 2023. When TRUE,
#'   the old version of \code{prepareTargetData} is used. When FALSE, the new version
#'   of \code{prepareTargetData} is used. When writing new code, you will get more
#'   accurate features using the new version of \code{prepareTargetData} because
#'   features are first projected to UTM and then buffers are generated. The old
#'   version of \code{prepareTargetData} applied buffers using the features in their
#'   native projection so if you were using a projection that did not preserve distances
#'   or areas, buffered shapes (e.g., points with a circular buffer) did not
#'   represent the correct area.
#' @param ... Additional arguments passed to \code{download.file}
#' @return A \code{SpatialPolygonsDataFrame} or \code{sf} object containing
#'   polygon(s) and attribute(s) for tiles covering the specified area.
#'   Attributes for the \code{aoi} are placed first in the \code{data.frame}
#'   followed by attributes for the lidar tile polygons..
#' @examples
#' \dontrun{
#' queryMPCTileIndex(-13540901, 5806426, 180, shape = "circle", crs = 3857)
#'   }
#' @export
queryMPCTileIndex <- function(
    x = NULL,
    y = NULL,
    buffer = 0,
    projectID = NULL,    #?
    fieldname = "workunit_id",      #?
    shape = "square",
    aoi = NULL,
    crs = "",
    segments = 60,
    return = "index",
    returnType = "sf",
    returncrs = "same",
    verbose = FALSE,
    useLegacyBuffering = FALSE,
    ...
) {
  # # check that we have something in the projectID
  # if (!length(projectID)) {
  #   stop("You must provide at least one project identifier in projectID.")
  # }

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

    if (verbose) message("--Projecting target features to WGS84")
    # transform to WGS84
    target84 <- sf::st_transform(target, 4326)

    # set attribute-geometry relationship to constant...all attributes represent the entire polygon
    sf::st_agr(target84) <- "constant"

    # get centroid...if multiple features, get centroid of unioned centroids
    pt <- sf::st_centroid(target84)
    if (nrow(pt) > 1) {
      pt <- unlist(sf::st_geometry(sf::st_centroid(sf::st_union(pt))))
    } else {
      pt <- unlist(sf::st_geometry(pt))
    }

    # calculate EPSG code...need to do this before buffer so we can use meters as units
    EPSG <- 32700 - round((45 + pt[2]) / 90, 0) * 100 + round((183 + pt[1]) / 6, 0)

    # transform to UTM
    targetUTM <- sf::st_transform(target84, EPSG)
  } else {
    targetUTM <- NULL
  }

  # prepare feature data for query...may be based on point (x,y) or
  # aoi (Spatial* or sf* object)
  if (verbose) message("--Preparing target data")
  targetUTM <- prepareTargetData(x = x, y = y, buffer = buffer, shape = shape,
                                 aoi = targetUTM, crs = crs, segments = segments,
                                 returnType = returnType, useLegacyBuffering = useLegacyBuffering)

  if (is.null(aoi)) {
    # just in case returnType is not sf, convert
    if (inherits(targetUTM, "Spatial")) {
      targetUTM <- sf::st_as_sf(targetUTM)
      convertTosp <- TRUE
    }

    # have x,y but it could be in a projection other than UTM
    if (verbose) message("--Projecting target feature to UTM")
    target84 <- sf::st_transform(targetUTM, 4326)

    # set attribute-geometry relationship to constant...all attributes represent the entire polygon
    sf::st_agr(target84) <- "constant"

    # get centroid...if multiple features, get centroid of unioned centroids
    pt <- sf::st_centroid(target84)
    if (nrow(pt) > 1) {
      pt <- unlist(sf::st_geometry(sf::st_centroid(sf::st_union(pt))))
    } else {
      pt <- unlist(sf::st_geometry(pt))
    }

    # calculate EPSG code for UTM zone
    EPSG <- 32700 - round((45 + pt[2]) / 90, 0) * 100 + round((183 + pt[1]) / 6, 0)

    # transform to UTM
    targetUTM <- sf::st_transform(target84, EPSG)
  } else {
    # project prepared aoi
    target84 <- sf::st_transform(targetUTM, 4326)
  }

  # set attribute-geometry relationship to constant...all attributes represent the entire polygon
  sf::st_agr(targetUTM) <- "constant"

  # query the MPC STAC index
  # simple approach is to use bounding box for all features in AOI
  # more complex approach creates geojson object from AOI and uses intersect= in call to stac_search

  bbox <- sf::st_bbox(target84)

  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
  rstac::get_request(s_obj)

  it_obj <- s_obj |>
    rstac::stac_search(collections = "3dep-lidar-copc",
                bbox = c(bbox[1], bbox[2], bbox[3], bbox[4]),
                limit = 1000) |>
    #  ext_query("3dep:usgs_id" == "IN_Statewide_Opt2_B4_2017") |>
    rstac::get_request()
#    rstac::items_sign(sign_fn = rstac::sign_planetary_computer())


  while (length(Filter(function(x) x$rel == "next", it_obj$links))) {
    it_obj <- rstac::items_next(it_obj)
  }

  if (is.null(it_obj)) return(NULL)
  if (length(it_obj) < 1) return(NULL)

  # get unsigned URL
  URLs <- rstac::assets_url(it_obj, asset_names = "data")

  it_obj <- rstac::items_sign(it_obj, sign_fn = rstac::sign_planetary_computer())
  signedURLs <- rstac::assets_url(it_obj, asset_names = "data")

  tiles <- rstac::items_as_sf(it_obj)

  # add URLS
  tiles$URL <- URLs
  tiles$signedURL <- signedURLs

  if (tolower(return) == "index") {
    # set return to target shapes with no target attribute information
    shortlist <- tiles
  } else if (tolower(return) == "aoi") {
    # intersect aoi shapes with tile shapes
    # set attribute-geometry relationship to constant...all attributes represent the entire polygon
    sf::st_agr(tiles) <- "constant"

    # intersect AOI with index
    #message("Warning messages (if any) from proj4string() regarding comments can be ignored...")
    prj <- sf::st_intersection(target84, tiles)

    shortlist <- prj
  } else {
    stop("invalid string for \"return\"": return)
  }

  if (!is.null(shortlist)) {
    if (nrow(shortlist) > 0) {
      # shortlist is always a sf object at this point
      if (tolower(returncrs) == "same") {
        # figure out the type provided using aoi and crs
        if (inherits(aoi, "Spatial")) {
          # aoi is Spatial* object
          if (verbose) message("--Projecting results: case 1")
          shortlist <- sf::st_transform(shortlist, crs=sf::st_crs(sf::st_as_sf(aoi)))
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

# ---------- signMPCURL
#
#' USGS Lidar Toolkit -- Sign lidar tile URLs using MPC access token
#'
#' Query Microsoft Planetary Computer (MPC) to obtain an access token and use it to sign
#' URLs for lidar point tiles. Function does not verify that the \code{URLs} point to
#' assets on MPC. It does, however, verify that \code{URLs} point to assets in
#' MPC blob storage. Only assets in MPC blob storage need access tokens. If \code{URLs}
#' do not point to assets in MPC blob storage, \code{URLs} are returned unchanged.
#'
#' Existing token are removed from \code{URLs} prior to signing.
#'
#' Signed URLs are only valid for the current R session and cannot be stored
#' for later use without resigning the URL.
#'
#' @param URLs character list containing unsigned URL(s) for point tiles.
#' @return A character list of signed URLs that can be used with \code{fetchUSGSTiles}
#'   or any other function that retrieves files using https protocol.
#' @examples
#' \dontrun{
#' signMPCURL(URLs)
#'   }
#' @export
signMPCURL <- function(
    URLs = NULL
) {
  if (is.null(URLs))
    stop("You must provide at least one URL to sign")

  getInfoInJson <- httr::GET('https://planetarycomputer.microsoft.com/api/sas/v1/token/3dep-lidar-copc',
                             httr::accept_json())

  # check the return
  if (!getInfoInJson$status_code == 200)
    stop("GET request for access token failed")

  # save the info in a json object
  jsonInfoImg <- httr::content(getInfoInJson, type="application/json")

  # loop through the list of URLs (could be a single item)
  # loop is not the best but I need to check each URL to make sure it is in blob storage
  ret <- vector("list", length(URLs))
  for (i in 1:length(URLs)) {
    # look for existing signing and remove
    t <- regexpr("\\?", URLs[i])
    if (t != -1)
      URLs[i] <- substr(URLs[i], 1, t - 1)

    # check that asset is in blob storage
    if (regexpr(".blob.core.windows.net", URLs[i], ignore.case = TRUE) == -1) {
      ret[i] = URLs[i]
    } else {
      ret[i] <- paste0(URLs[i], "?", jsonInfoImg$token)
    }
  }
  return(ret)
}
