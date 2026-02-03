test_that("build_ciwm_list applies weights and matches by name (Output: DF)", {
  # 1. Setup Trees
  es_tree <- list(
    provisioning = c("crops", "timber"),
    regulating   = c("carbon")
  )

  # 2. Setup Directory (Jumbled order to test name-matching)
  indicator_directory <- data.frame(
    ns_ci_num = c("ind2", "ind1"),
    provisioning = c(5, 10),       # ind1 weight is 10
    regulating   = c(20, 2),       # ind1 weight is 2
    stringsAsFactors = FALSE
  )

  # 3. Setup Binary Matrices
  mat_template <- matrix(1, nrow = 2, ncol = 3,
                         dimnames = list(c("oak", "pine"), c("crops", "timber", "carbon")))
  cirm_list <- list(ind1 = mat_template, ind2 = mat_template)

  # 4. Run Function (hab_tree is NULL, but function now returns DF regardless)
  res <- build_ciwm_list(cirm_list, indicator_directory, es_tree, habitats_label_tree = NULL)

  # 5. Verification
  # We check ind1: Provisioning should be 10, Regulating should be 2
  expect_equal(as.numeric(res$ind1["oak", "crops"]), 10)
  expect_equal(as.numeric(res$ind1["oak", "carbon"]), 2)

  # CRITICAL UPDATE: Check that it is a data frame, NOT a matrix
  expect_s3_class(res$ind1, "data.frame")
  expect_false(is.matrix(res$ind1))
})

test_that("build_ciwm_list handles NA weights (Output: DF)", {
  es_tree <- list(provisioning = "crops")
  indicator_directory <- data.frame(
    ns_ci_num = "ind1",
    provisioning = NA_real_,
    stringsAsFactors = FALSE
  )

  cirm_list <- list(ind1 = matrix(1, nrow = 1, ncol = 1, dimnames = list("oak", "crops")))

  res <- build_ciwm_list(cirm_list, indicator_directory, es_tree)

  # Verify it's a data frame and the value is NA
  expect_s3_class(res$ind1, "data.frame")
  expect_true(is.na(res$ind1[1, 1]))
})

test_that("build_ciwm_list throws error for missing IDs", {
  es_tree <- list(provisioning = "crops")
  indicator_directory <- data.frame(ns_ci_num = "missing_id", provisioning = 10)
  cirm_list <- list(ind1 = matrix(1, nrow = 1, ncol = 1, dimnames = list("oak", "crops")))

  expect_error(build_ciwm_list(cirm_list, indicator_directory, es_tree),
               "not found in directory")
})
