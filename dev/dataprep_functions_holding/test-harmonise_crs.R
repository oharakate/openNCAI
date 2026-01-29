# Objects for testing
# Create an object with a common CRS:
shape_crs4326 <- sf::st_sf(geom = sf::st_sfc(sf::st_point(c(0, 0))), crs = 4326)

# Create an object with a different CRS:
shape_crs32630 <- sf::st_sf(geom = sf::st_sfc(sf::st_point(c(0, 0))), crs = 32630)


test_that("harmonise_crs transforms CRS when needed", {

  # Input object has different CRS than AOI (WGS84 != UTM)
  result <- harmonise_crs(input_object = shape_crs4326, aoi_object = shape_crs32630)

  # Output must be an sf object
  expect_s3_class(result, "sf")

  # The CRS of the result must now match the CRS of the AOI (shape_crs32630)
  expect_true(sf::st_crs(result) == sf::st_crs(shape_crs32630))

  # A message should be printed
  expect_message(harmonise_crs(shape_crs4326,shape_crs32630),
                 regexp = "CRS mismatch detected. Transforming")
})

test_that("harmonise_crs does not transform identical CRS", {

  # Input object and AOI object have the same CRS (WGS84 == WGS84)
  result <- harmonise_crs(input_object = shape_crs4326, aoi_object = shape_crs4326)

  # Should not have transformed, so the CRS is still WGS84
  expect_true(sf::st_crs(result) == sf::st_crs(shape_crs4326))

  # Check 2: No transformation message should be printed
  expect_silent(harmonise_crs(shape_crs4326, shape_crs4326))
})

