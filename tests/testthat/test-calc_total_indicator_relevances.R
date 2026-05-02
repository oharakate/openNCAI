test_that("calc_total_indicator_relevances sums matrices correctly and applies constant", {
  # 1. Setup mock data
  # Two 2x2 matrices
  mat1 <- matrix(c(1, 2, 3, 4), nrow = 2)
  mat2 <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2)
  all_ciwms <- list(mat1, mat2)

  constant <- 2

  # 2. Run function
  result <- openNCAI:::calc_total_indicator_relevances(all_ciwms, total_indicator_relevances_constant = constant)

  # 3. Verification
  # Expected: (mat1 + mat2) + constant
  # Position [1,1]: (1 + 0.5) + 2 = 3.5
  # Position [2,2]: (4 + 0.5) + 2 = 6.5

  expect_equal(result[1, 1], 3.5)
  expect_equal(result[2, 2], 6.5)
  expect_equal(dim(result), c(2, 2))
})

test_that("calc_total_indicator_relevances works with a single matrix in the list", {
  mat1 <- matrix(1, nrow = 2, ncol = 2)
  result <- openNCAI:::calc_total_indicator_relevances(list(mat1), total_indicator_relevances_constant = 2)

  # Expected: 1 + 2 = 3
  expect_true(all(result == 3))
})

test_that("calc_total_indicator_relevances fails if matrix dimensions mismatch", {
  mat1 <- matrix(1, nrow = 2, ncol = 2)
  mat2 <- matrix(1, nrow = 3, ncol = 3) # Different size

  # Reduce("+", ...) will throw a base R error here
  expect_error(openNCAI:::calc_total_indicator_relevances(list(mat1, mat2), total_indicator_relevances_constant = 2))
})
