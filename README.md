
<!-- README.md is generated from README.Rmd. Please edit that file -->

# USGSlidar

<!-- badges: start -->

<!-- badges: end -->

The USGSlidar package was developed to make it somewhat easy to query
the collection of lidar data available from the U.S. Geological Survey’s
3D Elevation Program (and other sources) to find out if data are
available for specific locations of interest. The package accesses some
of the project and tile index files available from 3DEP and uses them to
search for data. The package also provides links to lidar data files
that can be downloaded for free directly from 3DEP servers.

The package also facilitates access to the USGS lidar collection
maintained in Entwine format. This collection contains a subset of the
data available from USGS but it is constantly being updated to match the
data available in the national map. The index for the Entwine collection
does not contain detailed project information so there is extra effort
required to marry the Entwine index with information in the USGS 3DEP
collection.

**This code should be considered experimental. The examples work but
some options for functions may not work as expected. I have used the
code for several months but I tend to use it the same way all the time
so I haven’t fully exercised all of the options for the functions. At
some point, I will add more tests and clean unp things that don’t
work.**

## Installation

This package is only distributed from my GitHub account. It may make it
to CRAN at some point but no promimses.

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
*build\_vignettes = FALSE*.

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("bmcgaughey1/USGSlidar", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(USGSlidar)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.
