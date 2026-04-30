test_that("label_ncai_matrix correctly applies labels to a raw matrix", {
  h_tree <- list(grassland = c("e1", "e2"), heathland = c("f1"))
  es_tree <- list(cultural = c("recreation", "education"))

  # Setup matrix with NO names (or wrong names)
  mat <- matrix(1:6, nrow = 3, ncol = 2)

  result <- label_ncai_matrix(mat, h_tree, es_tree)

  expect_s3_class(result, "data.frame")
  expect_equal(rownames(result), c("e1", "e2", "f1"))
  expect_equal(colnames(result), c("recreation", "education"))
  expect_equal(result["f1", "education"], 6)
})

test_that("label_ncai_matrix still catches dimension mismatches", {
  h_tree <- list(a = "h1")
  es_tree <- list(b = "es1")

  # Matrix with WRONG dimensions (2x1 instead of 1x1)
  mat <- matrix(c(1, 2), nrow = 2, ncol = 1)

  expect_error(
    label_ncai_matrix(mat, h_tree, es_tree),
    "Dimension mismatch!"
  )
})
