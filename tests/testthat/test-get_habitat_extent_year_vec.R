# Setup dummy data for testing
habitat_data <- matrix(
  c(100, 200, 300,  # 2000
    110, 210, 330,  # 2001
    50,  100, 150), # 2002
  nrow = 3,
  ncol = 3,
  dimnames = list(NULL, c("2000", "2001", "2002"))
)

test_that("get_habitat_extent_year_vec calculates correct index", {
  # 2001 vs 2000: (110/100)*100 = 110, (210/200)*100 = 105, (330/300)*100 = 110
  expected_output <- c(110, 105, 110)

  result <- get_habitat_extent_year_vec(
    target_year = 2001,
    year_one = 2000,
    habitat_extent = habitat_data
  )

  expect_equal(result, expected_output)
  expect_type(result, "double")
  expect_length(result, nrow(habitat_data))
})

test_that("Function handles character and numeric year inputs identically", {
  res_num <- get_habitat_extent_year_vec(2001, 2000, habitat_data)
  res_chr <- get_habitat_extent_year_vec("2001", "2000", habitat_data)

  expect_identical(res_num, res_chr)
})

test_that("Baseline year index (Target == Origin) returns 100", {
  result <- get_habitat_extent_year_vec(2000, 2000, habitat_data)
  expect_true(all(result == 100))
})

test_that("Errors are thrown for missing columns", {
  # Testing a year not in the matrix
  expect_error(
    get_habitat_extent_year_vec(2025, 2000, habitat_data),
    regexp = "subscript out of bounds"
  )
})

test_that("Handles zero values in origin year (Edge Case)", {
  # Adding a row with a 0 in the baseline to check for Inf results
  habitat_with_zero <- cbind(habitat_data, "2003" = c(0, 50, 100))

  result <- get_habitat_extent_year_vec(2000, 2003, habitat_with_zero)

  # 100 / 0 should result in Inf in R
  expect_true(is.infinite(result[1]))
})

