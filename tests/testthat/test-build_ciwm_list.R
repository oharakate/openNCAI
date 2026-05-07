test_that("build_ciwm_list applies weights and matches by name (Output: DF)", {
  # 1. Mock ES Tree - defines which services belong to which category
  es_tree <- list(
    provisioning = "crops",
    regulation_and_maintenance = "carbon",
    cultural = "recreation"
  )

  # 2. Mock Indicator Directory
  # Must contain columns matching the names(es_tree)
  indicator_directory <- data.frame(
    ci_id = c("ind1", "ind2"),
    provisioning = 0.5,
    regulation_and_maintenance = 0.2,
    cultural = 1.0,
    stringsAsFactors = FALSE
  )

  # 3. Setup Binary Relevance Matrices (CIRMs)
  # Rows = habitats, Cols = specific services (all_service_labels)
  mat_template <- matrix(1, nrow = 2, ncol = 3,
                         dimnames = list(c("oak", "pine"), c("crops", "carbon", "recreation")))

  # Ensure all indicators in this list exist in the indicator_directory above
  cirm_list <- list(ind1 = mat_template, ind2 = mat_template)

  # 4. Run Function
  res <- build_ciwm_list(cirm_list, indicator_directory, es_tree, habitats_label_tree = NULL)

  # 5. Verification
  # Calculation: Binary(1) * Weight(0.5) = 0.5
  expect_equal(as.numeric(res$ind1["oak", "crops"]), 0.5)
  # Calculation: Binary(1) * Weight(0.2) = 0.2
  expect_equal(as.numeric(res$ind1["oak", "carbon"]), 0.2)

  # Check that it returned a data frame as requested
  expect_s3_class(res$ind1, "data.frame")
  expect_false(is.matrix(res$ind1))
})

test_that("build_ciwm_list handles NA weights (Output: DF)", {
  es_tree <- list(provisioning = "crops")

  indicator_directory <- data.frame(
    ci_id = "ind1",
    provisioning = NA_real_,
    stringsAsFactors = FALSE
  )

  cirm_list <- list(ind1 = matrix(1, nrow = 1, ncol = 1, dimnames = list("oak", "crops")))

  res <- build_ciwm_list(cirm_list, indicator_directory, es_tree)

  # Verify it's a data frame and the value remains NA
  expect_s3_class(res$ind1, "data.frame")
  expect_true(is.na(res$ind1[1, 1]))
})

test_that("build_ciwm_list throws error for missing IDs", {
  es_tree <- list(provisioning = "crops")

  # Directory has "missing_id", but list has "ind1" -> This should trigger the stop()
  indicator_directory <- data.frame(ci_id = "missing_id", provisioning = 10)
  cirm_list <- list(ind1 = matrix(1, nrow = 1, ncol = 1, dimnames = list("oak", "crops")))

  expect_error(build_ciwm_list(cirm_list, indicator_directory, es_tree),
               "not found in directory")
})
