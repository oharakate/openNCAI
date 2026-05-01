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
library(sinew)
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
use_package("dplyr")
use_package("readxl")
use_package("janitor")
use_package("stats")
use_package("tidyr")
use_package("slider")
use_package("magrittr")
use_package("stringr")
use_package("openxlsx")

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


#### Importing NS data and bundling as rdf ####
use_data_raw(name = "scotNCAI_data")

# Put the spreadsheet in /data-raw but don't export.
# Leave the function in place and the user can use their own path.
# We can use this one for testing.
use_build_ignore("data-raw/NatureScot_NCAI_Template.xlsx")

# Use rprojroot tool to help with tests involving the spreadsheet:
usethis::use_package("rprojroot", type = "Suggests")


#### Building the NS vignette ####
use_vignette("replicating_scotlands_ncai")




#### Putting functions into the package ####

#### label_ncai_matrix()####

use_r("label_ncai_matrix")
# Remember to put in the roxygen skeleton
load_all()
document()

# Tests
use_test("label_ncai_matrix")
load_all()
test_file("tests/testthat/test-label_ncai_matrix.R")
document()

load_all()
check()
# Commit and push here.


#### get_ns_data() ####
use_r("import_ns_data")
load_all()
document()
check()
use_test("import_ns_data")
load_all()
test_file("tests/testthat/test-import_ns_data.R")

#### get_ns_testing_data ####
use_r("import_ns_testing_data")
use_test("import_ns_testing_data")
load_all()
document()
test_file("tests/testthat/test-import_ns_testing_data.R")
check()

#### esppu_scores_to_weights()####

use_r("calc_potential_weights")
# Remember to put in the roxygen skeleton
load_all()
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
document()

# Tests
use_test("calc_importance_weights")
load_all()
test_file("tests/testthat/test-calc_importance_weights.R")
document()

load_all()
check()
# Commit and push here.



#### calc_wellbeing_base()####

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



#### calc_tir()####

use_r("calc_tir")
# Remember to put in the roxygen skeleton
load_all()
?calc_tir
document()

# Tests
use_test("calc_tir")
load_all()
test_file("tests/testthat/test-calc_tir.R")
document()

load_all()
check()
# Commit and push here.


#### build_all_tyfs()####

use_r("build_all_tyfs")
# Remember to put in the roxygen skeleton
load_all()
document()
?build_all_tyfs
?build_tyf

# Tests
use_test("build_all_tyfs")
load_all()
test_file("tests/testthat/test-build_all_tyfs.R")
document()

load_all()
check()
# Commit and push here.


#### get_yearly_flow() ####
use_r("get_yearly_flow")
# Remember to put in the roxygen skeleton
load_all()
document()
?get_yearly_flow

# Tests
use_test("get_yearly_flow")
load_all()
test_file("tests/testthat/test-get_yearly_flow.R")
document()

load_all()
check()
# Commit and push here.


#### build_all_ncai_matrices()####

use_r("build_all_ncai_matrices")
# Remember to put in the roxygen skeleton
load_all()
document()
?build_ncai_matrix
?build_all_ncai_matrices

# Tests
use_test("build_all_ncai_matrices")
load_all()
test_file("tests/testthat/test-build_all_ncai_matrices.R")
document()

load_all()
check()
# Commit and push here.


#### calc_ncai()####

use_r("calc_ncai")
# Remember to put in the roxygen skeleton
load_all()
document()
?calc_ncai
?calc_ncai_by_st
?calc_ncai_by_bh

# Tests
use_test("index_and_smooth")
load_all()
test_file("tests/testthat/test-index_and_smooth.R")
?index_and_smooth
document()

load_all()
check()
# Commit and push here.#### template()####



#### template() ####
use_r("get_yearly_potentials")
# Remember to put in the roxygen skeleton
load_all()
document()
?get_yearly_potential_provision
?get_yearly_potential_wellbeing

use_test("get_yearly_potentials")
load_all()
test_file("tests/testthat/test-get_yearly_potentials.R")
load_all()
check()
# Commit and push here.



#### get_ncai() ####
use_r("get_ncai")
# Remember to put in the roxygen skeleton
load_all()
document()
?get_ncai

# Tests
use_test("get_ncai")
load_all()
test_file("tests/testthat/test-get_ncai.R")
document()

load_all()
check()
# Commit and push here.




#### template() ####
use_r("get_habitat_extent_year_vec")
# Remember to put in the roxygen skeleton
load_all()
document()
?get_habitat_extent_year_vec

# Tests
use_test("get_habitat_extent_year_vec")
load_all()
test_file("tests/testthat/test-get_habitat_extent_year_vec.R")
document()

load_all()
check()
# Commit and push here.


#### make_and_read_data_template() ####
use_r("make_and_read_data_template")
# Remember to put in the roxygen skeleton
load_all()
document()
?create_ncai_template
?read_ncai_template

# Tests
use_test("make_and_read_template")
load_all()
test_file("tests/testthat/test-make_and_read_template.R")
document()

load_all()
check()
# Commit and push here.


#### template() ####
use_r("X")
# Remember to put in the roxygen skeleton
load_all()
document()
?X

# Tests
use_test("X")
load_all()
test_file("tests/testthat/test-X.R")
document()

load_all()
check()
# Commit and push here.

# Run all examples
devtools::run_examples()

# # End things
# # We can install the package with
# install()
#
# library(openNCAI)
# # And then get rid to continue developing with
# remove.packages("openNCAI")



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
#
