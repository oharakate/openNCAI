# This is Kate's jotter for doing the packaging things in.

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# We will probably change the name of this package at some point.
# This website might help with that:
# https://www.njtierney.com/post/2017/10/27/change-pkg-name/

# I previously worked through this:
# https://r-pkgs.org/whole-game.html

# use_this docs:
# https://usethis.r-lib.org/
# test_that docs:
# https://testthat.r-lib.org/
# roxygen docs:
# https://roxygen2.r-lib.org/

#### SETUP ####
# Install packages
# install.packages("devtools")
# install.packages("covr")

# Check devtools version
# package?devtools
# We're looking for 2.4.6 to match the website guide.

# Load packages for development
library(devtools)
library(gitcreds)
library(covr)
library(dplyr)
library(tidyr)
library(terra)
library(sf)


#### ONE-TIME PACKAGE SETUP TASKS ####
# Create the package
# create_package("~/habitats/openNCAI")

# Declare these dependencies
# use_package("sf")
# use_package("terra")
# use_package("tidyr")
# use_package("dplyr")

# Use Git
# usethis::use_git_config(
#   user.name = "oharakate",
#   user.email = "sadkate@gmail.com"
# )
# use_git()

# Use Github
# create_github_token()
# gitcreds_set()

# use_github()

# Ignore this notebook and everything in dev:
# usethis::use_build_ignore("dev/")

# Use MIT licence
# use_mit_license("Kate O'Hara")
# Also manually update the description at this point.

# Edit DESCRIPTION

# Generate documentation
# document()

# Use testthat()
# use_testthat()
####




#### Putting functions into the package ####
# load_shape()
use_r("load_shape")
load_all()

# Call this on some test data
load_shape("inst/extdata/test_data.shp")
load_shape("inst/extdata/test_aoi.shp")
# Looks OK.

use_test("load_shape")
test_file("tests/testthat/test-load_shape.R")
package_coverage()

# At the end of session, remember to:
# Do Code > Insert roxygen skeleton inside the function
# And edit it

# Also, to get update the manual

document()

# Check the package:
check()

# To use the test data in an example it was necessary to specify its location in
# inst/testdata in the example in man/ like this:
shapefile_path <- system.file(
  "extdata",
  "test_data.shp", # Use the name of your test shapefile
  package = "openNCAI"
)
load_shape(shapefile_path)
# I don't think it looks nice in the example but it does work. I'll ask Chris
# what he thinks.


## harmonise_crs()

use_r("harmonise_crs")
# Remember to put in the roxygen skeleton
load_all()
document()

# Tests
use_test("harmonise_crs")
load_all()
test_file("tests/testthat/test-harmonise_crs.R")
document()

load_all()
check()
# Commit and push here.

##



# End things
# We can install the package with
install()
library(openNCAI)
# And then get rid to continue developing with
remove.packages("openNCAI")
