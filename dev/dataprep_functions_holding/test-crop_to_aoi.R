# Some data for testing:

# Make a big square (e.g., 20km x 20km area)
# Coords are in meters (e.g., Easting 400000 to 420000)
easting_min <- 400000
easting_max <- 420000
northing_min <- 300000
northing_max <- 320000

extent_data <- sf::st_polygon(list(matrix(
  c(easting_min, northing_min,
    easting_max, northing_min,
    easting_max, northing_max,
    easting_min, northing_max,
    easting_min, northing_min),
  ncol = 2, byrow = TRUE)))
habitat_sfc <- sf::st_sfc(extent_data, crs = 27700)
big_square <- sf::st_sf(id = 1, geom = habitat_sfc)

# Make a smaller circle which is inside big square
sc1_centre_point <- sf::st_point(c(410000, 310000))
sc1_centre <- sf::st_sfc(sc1_centre_point, crs = 27700)
# Creating a circular AOI by buffering the point by 4000 meters (4 km radius)
aoi_in_square <- sf::st_buffer(sc1_centre, dist = 4000)
aoi_in_square <- sf::st_sf(id = 1, geom = aoi_in_square)

# Make a small circle not in the big square
sc2_centre_point <- sf::st_point(c(500000, 500000))
sc2_centre <- sf::st_sfc(sc2_centre_point, crs = 27700)
# Buffer by 2000 meters (2 km radius)
aoi_out_square <- sf::st_buffer(sc2_centre, dist = 2000)
aoi_out_square <- sf::st_sf(id = 1, geom = aoi_out_square)


# Make a big circle which all of the big square is inside
sc3_centre_point <- sf::st_point(c(410000, 310000))
sc3_centre <- sf::st_sfc(sc3_centre_point, crs = 27700)
# Buffer by 15000 meters (15 km radius) - covers the 20km square diagonally
aoi_ate_square <- sf::st_buffer(sc3_centre, dist = 15000)
aoi_ate_square <- sf::st_sf(id = 1, geom = aoi_ate_square)


## TESTS

test_that("crop_to_aoi() returns a valid sf object", {
  result <- suppressWarnings(
    openNCAI::crop_to_aoi(big_square, aoi_in_square)
  )

  expect_s3_class(result, "sf")
})

test_that("a larger extent shape is cropped to the smaller extent of the aoi", {

  cropped <-  suppressWarnings(
    openNCAI::crop_to_aoi(big_square, aoi_in_square)
  )

  expected_bbox <- sf::st_bbox(aoi_in_square)
  actual_bbox   <- sf::st_bbox(cropped)

  expect_equal(actual_bbox, expected_bbox, tolerance = 1e-6,
               info = "The cropped result extent does not match the AOI extent."
  )
})

test_that("the input shape is not cropped when the input shape is contained
          within the AOI", {
            result <- suppressWarnings(
              openNCAI::crop_to_aoi(big_square, aoi_ate_square)
            )

            bbox_before = sf::st_bbox(big_square)
            bbox_after  = sf::st_bbox(result)

            expect_equal(bbox_after, bbox_before, tolerance = 1e06,
                         info = "The cropped result is changed but it shouldn't
                         have been.")
              })


test_that("an empty shape is returned if the aoi boundary does not intersect
          the extent data", {
            result <- suppressWarnings(
              openNCAI::crop_to_aoi(big_square, aoi_out_square)
            )

            expect_equal(nrow(result), 0, tolerance = 0)

          })

