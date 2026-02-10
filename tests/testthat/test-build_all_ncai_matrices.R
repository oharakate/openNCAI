test_that("build_ncai_matrix correctly combines TYF, Wellbeing, and Extent", {
  tyf <- matrix(100, nrow = 2, ncol = 2)

  # Ensure the wellbeing base has the row names we are testing for!
  wb <- matrix(100, nrow = 2, ncol = 2,
               dimnames = list(c("hab1", "hab2"), NULL))

  extent_df <- data.frame(
    "2000" = c(100, 50),
    "2001" = c(100, 100),
    row.names = c("hab1", "hab2"),
    check.names = FALSE
  )

  res <- build_ncai_matrix(tyf, wb, extent_df, "2001", "2000")

  # Use as.numeric() to ignore potential name attributes on the single value
  expect_equal(as.numeric(res["hab1", 1]), 100)
  expect_equal(as.numeric(res["hab2", 1]), 200)
})

test_that("build_all_ncai_matrices returns named list of data frames", {
  # Minimal setup
  tyf_list <- list("2000" = matrix(100, 1, 1))
  wb <- matrix(100, 1, 1)
  extent <- data.frame("2000" = 100, row.names = "hab1", check.names = FALSE)
  labels <- "Woodland"

  results <- build_all_ncai_matrices(tyf_list, wb, extent, "2000", labels)

  expect_type(results, "list")
  expect_named(results, "2000")
  expect_s3_class(results[[1]], "data.frame")
  expect_equal(rownames(results[[1]]), "Woodland")
})
