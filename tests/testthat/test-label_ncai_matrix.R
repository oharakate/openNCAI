test_that("label_ncai_matrix correctly applies expanded labels", {
  # 1. Setup trees
  h_tree <- list(
    grassland = c("e1", "e2"),
    heathland = c("f1")
  )
  es_tree <- list(
    cultural = c("recreation", "education")
  )

  # 2. Setup matrix
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  rownames(mat) <- c("e1", "e2", "f1")
  colnames(mat) <- c("recreation", "education")

  # 3. Execute
  result <- label_ncai_matrix(mat, h_tree, es_tree)

  # 4. Verify
  expect_s3_class(result, "data.frame")
  expect_equal(rownames(result), c("e1", "e2", "f1"))
  expect_equal(result["f1", "education"], 6)
})

test_that("label_ncai_matrix enforces strict matching (Error Handling)", {
  h_tree <- list(a = "h1")
  es_tree <- list(b = "es1")

  # Matrix with WRONG names
  mat <- matrix(100)
  rownames(mat) <- "wrong_name"
  colnames(mat) <- "es1"

  # Verify the function stops with the specific error message
  expect_error(
    label_ncai_matrix(mat, h_tree, es_tree),
    "Mismatch: The row names in the data do not match"
  )
})

test_that("label_ncai_matrix reorders rows to match the tree order", {
  # Tree order is e1 then e2
  h_tree <- list(grass = c("e1", "e2"))
  es_tree <- list(serv = "s1")

  # Matrix provided in REVERSE order (e2 then e1)
  mat <- matrix(c(20, 10), nrow = 2)
  rownames(mat) <- c("e2", "e1")
  colnames(mat) <- "s1"

  result <- label_ncai_matrix(mat, h_tree, es_tree)

  # The output should follow the tree order (e1, then e2)
  expect_equal(rownames(result), c("e1", "e2"))
  expect_equal(result["e1", "s1"], 10)
})
