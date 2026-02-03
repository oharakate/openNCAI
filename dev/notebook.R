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
# library(gitcreds)
# library(covr)
# library(dplyr)
# library(tidyr)
# library(terra)
# library(sf)


#### ONE-TIME PACKAGE SETUP TASKS ####
# Create the package
# create_package("~/habitats/openNCAI")

# Declare these dependencies
# use_package("sf")
# use_package("terra")
# use_package("tidyr")
use_package("dplyr")
# use_package("tibble")
# use_package("readr")
# use_package("readxl")
# use_package("slider")
# use_package("ggplot2")

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

#### label_ncai_matrix()####

use_r("label_ncai_matrix")
# Remember to put in the roxygen skeleton
load_all()
run_examples(pkg = ".", test = "label_ncai_matrix")
document()

# Tests
use_test("label_ncai_matrix")
load_all()
test_file("tests/testthat/test-label_ncai_matrix.R")
document()

load_all()
check()
# Commit and push here.


#### esppu_scores_to_weights()####

use_r("calc_potential_weights")
# Remember to put in the roxygen skeleton
load_all()
run_examples(pkg = ".", test = "calc_potential_weights")
document()

# Tests
use_test("calc_potential_weights")
load_all()
test_file("tests/testthat/test-calc_potential_weights.R")
document()

load_all()
check()
  # Commit and push here.
 ## THINK ABOUT LABELLING THIS DF?


#### calc_espb()####

use_r("calc_espb")
# Remember to put in the roxygen skeleton
load_all()
run_examples(pkg = ".", test = "calc_espb")
document()

# Tests
use_test("calc_espb")
load_all()
test_file("tests/testthat/test-calc_espb.R")
document()

load_all()
check()
# Commit and push here.


#### calc_importance_weights()####

use_r("calc_importance_weights")
# Remember to put in the roxygen skeleton
load_all()
run_examples(pkg = ".", test = "calc_importance_weights")
document()

# Tests
use_test("calc_importance_weights")
load_all()
test_file("tests/testthat/test-calc_importance_weights.R")
document()

load_all()
check()
# Commit and push here.


#### bind_importance_weights()####

use_r("bind_importance_weights")
# Remember to put in the roxygen skeleton
load_all()
run_examples(pkg = ".", test = "bind_importance_weights")
document()

# Tests
use_test("bind_importance_weights")
load_all()
test_file("tests/testthat/test-bind_importance_weights.R")
document()

load_all()
check()
# Commit and push here.


#### template()####

use_r("calc_wellbeing_base")
# Remember to put in the roxygen skeleton
load_all()
document()

# Tests
use_test("calc_wellbeing_base")
load_all()
test_file("tests/testthat/test-calc_wellbeing_base.R")
document()

load_all()
check()
# Commit and push here.


#### build_ciwm_list()####

use_r("build_ciwm_list")
# Remember to put in the roxygen skeleton
load_all()
?build_ciwm_list
document()

# Tests
use_test("build_ciwm_list")
load_all()
test_file("tests/testthat/test-build_ciwm_list.R")
document()

load_all()
check()
# Commit and push here.



#### template()####

use_r("X")
# Remember to put in the roxygen skeleton
load_all()
?X
document()

# Tests
use_test("X")
load_all()
test_file("tests/testthat/test-X.R")
document()

load_all()
check()
# Commit and push here.




# End things
# We can install the package with
install()

library(openNCAI)
# And then get rid to continue developing with
remove.packages("openNCAI")



#### DATAPREP FUNCTIONS THAT WERE REMOVED FOR NOW ####

# ### load_shape() ####
# use_r("load_shape")
# load_all()
#
# # Call this on some test data
# load_shape("inst/extdata/test_data.shp")
# load_shape("inst/extdata/test_aoi.shp")
# # Looks OK.
#
# use_test("load_shape")
# test_file("tests/testthat/test-load_shape.R")
#
#
# # At the end of session, remember to:
# # Do Code > Insert roxygen skeleton inside the function
# # And edit it
#
# # Also, to get update the manual
#
# document()
#
# # Check the package:
# check()
#
#
# #### harmonise_crs()####
#
# use_r("harmonise_crs")
# # Remember to put in the roxygen skeleton
# load_all()
# document()
#
# # Tests
# use_test("harmonise_crs")
# load_all()
# test_file("tests/testthat/test-harmonise_crs.R")
# document()
#
# load_all()
# check()
# # Commit and push here.
#
# #### crop_to_aoi() ####
# use_r("crop_to_aoi")
# load_all()
# document()
#
# # tests
# use_test("crop_to_aoi")
# load_all()
# test_file("tests/testthat/test-crop_to_aoi.R")
#
# document()
# check()
# # Commit and push here.
