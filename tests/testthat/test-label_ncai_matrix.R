test_that("label_ncai_matrix correctly applies expanded labels", {
  # 1. Setup trees
  h_tree <- list(
    grassland = c("e1", "e2"),
    heathland = c("f1")
  )
  es_tree <- list(
    cultural = c("recreation", "education")
  )

  # 2. Setup matrix (3 rows for habitats, 2 columns for services)
  mat <- matrix(1:6, nrow = 3, ncol = 2)

  # 3. Execute
  result <- label_ncai_matrix(mat, h_tree, es_tree)

  # 4. Verify output type
  expect_s3_class(result, "data.frame")

  # 5. Verify Row Labels (expanded from list values)
  expect_equal(rownames(result), c("e1", "e2", "f1"))

  # 6. Verify Column Labels (expanded from list values)
  expect_equal(colnames(result), c("recreation", "education"))

  # 7. Verify Data Integrity (Ensure values didn't shift)
  expect_equal(result["e1", "recreation"], 1)
  expect_equal(result["f1", "education"], 6)
})

test_that("label_ncai_matrix handles empty trees gracefully", {
  # Testing with single elements
  h_tree <- list(a = "h1")
  es_tree <- list(b = "es1")
  mat <- matrix(100)

  result <- label_ncai_matrix(mat, h_tree, es_tree)

  expect_equal(rownames(result), "h1")
  expect_equal(colnames(result), "es1")
})
