
<!-- README.md is generated from README.Rmd. Please edit that file -->

# USGSlidar

<!-- badges: start -->
<!-- badges: end -->

The USGSlidar package was developed to make it somewhat easy to query
the collection of lidar data available from the U.S. Geological Survey’s
3D Elevation Program (and other sources) to find out if data are
available for specific locations of interest. The package offers options
to access the project and tile index files available from 3DEP and uses
them to search for data. The package also provides links to lidar data
files that can be downloaded for free directly from 3DEP servers.

The package also facilitates access to the USGS lidar collection
maintained in Entwine format. This collection contains a subset of the
data available from USGS but it is constantly being updated to match the
data available in the national map. The index for the Entwine collection
does not contain detailed project information so there is extra effort
required to marry the Entwine index with information in the USGS 3DEP
collection. I also maintain a companion repository
[EntwineIndex](https://github.com/bmcgaughey1/EntwineIndex) that merges
the Entwine index created and maintained by Howard Butler
[USGS-lidar](https://github.com/hobu/usgs-lidar) with the USGS project
index to add project information to the Entwine index. My enhanced
Entwine index can be accesses by setting type = “entwineplus” in the
call to fetchUSGSProjectIndex().

When working with the Entwine collection, the USGSlidar package provides
functions to create PDAL pipelines to download data for very specific
areas. My general use case involves data covering forestry plots. I want
data for areas that are generally less than 1 ha. Using Entwine data and
PDAL allows you to download only data covering a specific extent
regardless of the structure of the point data. When working with the
3DEP data collection, you have to download all point tiles covering an
area of interest and then clip data covering your specific extent. In
general, using the Entwine data collection and PDAL pipelines is much
more efficient compared to downloading full tiles and then clipping data
for a specific extent. However, the end result is the same (provided you
delete all the point tiles necessary to cover the desired area(s) after
clipping data for specific areas).

The package also supports querying the lidar data collection hosted on
Microsoft Planetary Computer (MPC). This collection is a snapshot of the
USGS 3DEP data capture late in 2022. Projects added to the USGS and
Entwine collections after the snapshot date are not included in the MPC
collection. The MPC data are organized as individual point tiles with
attributes for each tile that include the lidar project name, projection
information, and data collection start and end dates). The point data
were projected to UTM for the snapshot with the UTM zone appropriate for
the data location (center of the lidar project area).

**This code should be considered experimental. The examples work but
some options for functions may not work as expected. I have used the
code for several months but I tend to use it the same way all the time
so I haven’t fully exercised all of the options for the functions. At
some point, I will add more tests and clean up things that don’t work.**

## Installation

This package is only distributed from my GitHub account. It may make it
to CRAN at some point but no promises.

~~You can install the released version of USGSlidar from
[CRAN](https://CRAN.R-project.org) with:~~

``` r
#install.packages("USGSlidar")
```

**USGSlidar** is currently available as a development version only. The
**devtools** package is required to install **USGSlidar**. If you have
not previously used **devtools**, use the commented line of code to
install the package. Note that this will also install several additional
packages needed for devtools. If you do not want the vignettes, set
*build_vignettes = FALSE*.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("bmcgaughey1/USGSlidar", build_vignettes = TRUE)
```

## Potential problems with USGS WESM and TESM index files

In 2022, the USGS TESM tile index has problems with some lidar projects.
The problems were related to missing or bad geometries for some tiles
and involved \~20 lidar projects (haven’t checked non-lidar projects).
The queryUSGSTileIndex() function works correctly when the project being
queried has good tiles but it may fail when there are missing or bad
tile geometries within the project. USGS corrected these problems as
they were discovered but I don’t know if all projects have been
corrected. However, I have used the TESM index for several projects
lately and have not encountered any problems. Hopefully, this means that
all problems with the index have been fixed.

The TESM tile index does not include sufficient information to fully
locate the tile (LAS/LAZ file) on the rockyweb server. The tile_id field
in the TESM index is just a tile identifier and does not (usually) fully
identify the file on the server. The logic used to create the actual
file names seems to vary depending on the lidar project. For recent
projects, the actual file names appear to be constructed using the
project name and the year the point data were published. Older projects
do not include information for the publication year in the file names.

While the WESM index contains a lpc_link field that is presumably the
URL for the point files associated with a project, the URL is incomplete
and does not include the actual folder containing the point files.
Usually, the point files are in a folder named “laz” or “LAZ” but some
projects have point files in a folder named “LAS”. The lpc_link URLs are
often missing the trailing “/” so code that uses them needs to check for
the trailing “/” and add it if missing.

### Example

The data for Glacier Peak in Washington state is identified in WESM by:

-   workunit = “WA_GlacierPeak_2014”
-   workunit = 18330
-   project = “Glacier_Peak_WA_QL1_LiDAR”
-   project_id = 18332
-   lpc_pub_date = “2016-08-08”
-   lpc_link =
    “<https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/USGS_LPC_WA_GlacierPeak_2014_LAS_2016/>”

Data for this area were collected in 2014-2015 and published in 2016.
The point data files area actually located in
“<https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/USGS_LPC_WA_GlacierPeak_2014_LAS_2016/laz/>”
and individual files names look like this:
“USGS_LPC_WA_GlacierPeak_2014_10TFU1514_LAS_2016.laz”

The record in the TESM tile index for the same tile contains the
following:

-   tile_id = “10TFU1514”
-   project = “Glacier_Peak_WA_QL1_LiDAR”
-   project_id = 18332
-   workunit_id = 18330

For this project and tile, we can construct a URL as follows (R syntax
using lubridate package for year() function): URL \<-
paste0(WESM\$lpc_link, “laz/”, “USGS_LPC\_”, WESM\$workunit, “\_“,
TESM\$tile_id,”\_LAS\_“, year(WESM\$lpc_pub_date),”.laz”)

For projects where the lpc_pub_date is missing of set to NA, the URL may
be as follows (but not tested for all projects): URL \<-
paste0(WESM\$lpc_link, “laz/”, “USGS_LPC\_”, WESM\$workunit, “\_“,
TESM\$tile_id,”.laz”)

## Example – See FIAPlotExample.R in the ExampleScripts folder

The FIAPlotExample.R script queries the USGS Entwine data collection to
find data covering the locations of FIA (Forest Inventory and Analysis)
plots. It then constructs PDAL pipelines to retrieve data covering the
plots using the published locations. To run the pipelines, you will need
an Anaconda environment configured to run PDAL pipelines. The PDAL
workshop materials provide instructions: [PDAL
workshop](https://pdal.io/workshop/conda.html#installing-conda).

Once you have your conda environment running with an environment that
includes PDAL, you can run the pipelines created by the FIAPlotEsample.R
script by running RunME.bat from the folder containing the pipelines.

``` r
library(USGSlidar)

# basic example code
```
