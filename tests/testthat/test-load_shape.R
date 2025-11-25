test_that("load_shape_works", {

  # Identify the path to a valid file
  test_file_path <- file.path("test_data", "test_data.shp")

  # Use the valid file for the test:
  shape_obj <- load_shape(test_file_path)

  # We check it's of "sf", the expected class
  expect_s3_class(shape_obj, "sf")
})

# Examples for me to work from...
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
#
#
# test_that("str_split_one() splits a string", {
#   expect_equal(str_split_one("a,b,c", ","), c("a", "b", "c"))
# })
#
# test_that("str_split_one() errors if input length > 1", {
#   expect_error(str_split_one(c("a,b","c,d"), ","))
# })
#
# test_that("str_split_one() exposes features of stringr::str_split()", {
#   expect_equal(str_split_one("a,b,c", ",", n = 2), c("a", "b,c"))
#   expect_equal(str_split_one("a.b", stringr::fixed(".")), c("a", "b"))
# })
