test_that("import_ns_data returns the correct structure", {
  # Setup: Point to the template in your package
  path <- system.file("extdata", "ncai.xlsx", package = "openNCAI")

  # Skip tests if the file isn't there (avoids failure during automated builds)
  skip_if(path == "")

  years <- 2000:2022
  result <- import_ns_data(path, year_list = years)

  # 1. Test overall output type
  expect_type(result, "list")

  # 2. Test Dimensions
  # Habitat extent should have as many rows as total habitats
  expect_equal(nrow(result$ns_habitat_extent), length(result$ns_all_habitat_labels))
  # Habitat extent columns should match the number of years
  expect_equal(ncol(result$ns_habitat_extent), length(years))

  # 3. Test ESPPU and Custom Divisor alignment
  # They must be the exact same shape for division logic to work
  expect_equal(dim(result$ns_esppu), dim(result$ns_custom_divisor_matrix))
})

test_that("make_custom_divisor_matrix handles partial matching correctly", {
  # Mock data
  habitats <- c("b1_coastal", "b2_woodland", "c1_water")
  services <- c("timber_1", "crops_2", "climate_3")

  # Test if "b1" shorthand correctly matches "b1_coastal"
  res <- make_custom_divisor_matrix(
    all_habitat_labels = habitats,
    all_es_labels = services,
    habitats_to_adjust = "b1",
    services_to_adjust = "timber",
    usual_divisor = 5,
    custom_divisor = 1
  )

  # The first cell (b1, timber) should be 1
  expect_equal(res[[1, 1]], 1)
  # Others should remain 5
  expect_equal(res[[2, 2]], 5)
})

test_that("get_ns_cirm_list produces binary values", {
  path <- system.file("extdata", "ncai.xlsx", package = "openNCAI")
  skip_if(path == "")

  # Test the helper directly
  # (Using ::: since it's an internal function)
  cirms <- openNCAI:::get_ns_cirm_list(
    path = path,
    sheet_list = 9,
    matrix_range = "F4:AG34",
    ci_ids = "test_id",
    all_service_labels = paste0("service_", 1:28)
  )

  # Check that all values are either 0 or 1
  val_check <- all(unlist(cirms[[1]]) %in% c(0, 1))
  expect_true(val_check)
})
