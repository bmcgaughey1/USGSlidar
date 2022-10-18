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
#'   The pipeline templates provided with the USGSlidar package are stored in the install folder
#'   for the package under the \code{extdata} folder. While you can directly modify these copies
#'   of the files, it would be better to copy them to another location, make the desired
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
