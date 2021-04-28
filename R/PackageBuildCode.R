# notes and code for building package
#
# I ran into problems building and installing the package related to the vignette.
# Run devtools::install(build_vignettes = TRUE) but make sure the folder for the package isn't
# open in file explorer or a command prompt. Otherwise, the install will fail!!
library(devtools)

setwd("G:/R_Stuff/USGSlidar")
devtools::install(build_vignettes = TRUE)

# check for vignette
browseVignettes("USGSlidar")

# rebuild readme.md
devtools::build_readme()

# build vignettes and copy them to inst/doc in project
# this should make them available when installing from github
tools::buildVignettes(dir = ".", tangle=TRUE)
dir.create("inst")
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
