# --- SETUP ---
# test_path looks up from tests/testthat/ to project root, then into data-raw/
# (robust way to handle external files in testthat)
path <- testthat::test_path("..", "..", "data-raw", "ncai_corrected.xlsx")

# --- TESTS ---

test_that("import_ns_data returns the correct 11-component list structure", {
  # Skip this during R CMD check if the file is not reachable
  skip_if_not(file.exists(path), message = "Spreadsheet not found (expected during R CMD check)")

  years <- 2000:2022
  result <- openNCAI:::import_ns_data(path = path, year_list = years)

  # 1. Check overall structure
  expect_type(result, "list")
  expect_length(result, 11)

  # 2. Verify all named components exist
  expected_names <- c(
    "ns_habitat_extent", "ns_ci_scores", "ns_habitats_label_tree",
    "ns_es_label_tree", "ns_year_list", "ns_provision_per_uniot_scores", "ns_custom_divisor_matrix",
    "ns_between_importance_scores", "ns_within_importance_scores",
    "ns_ci_relevance_matrices", "ns_indicator_directory"
  )
  expect_setequal(names(result), expected_names)
})

test_that("Label Trees and Matrices are perfectly aligned", {
  skip_if_not(file.exists(path), message = "Spreadsheet not found")

  result <- openNCAI:::import_ns_data(path = path)

  # Flatten labels for testing
  all_habitats <- unlist(result$ns_habitats_label_tree, use.names = FALSE)
  all_services <- unlist(result$ns_es_label_tree, use.names = FALSE)

  # 1. Check Habitat Extent Alignment
  expect_equal(nrow(result$ns_habitat_extent), length(all_habitats))
  expect_equal(rownames(result$ns_habitat_extent), all_habitats)
  expect_equal(colnames(result$ns_habitat_extent), result$ns_year_list)

  # 2. Check Provision Per Unit Alignment
  expect_equal(nrow(result$ns_provision_per_uniot_scores), length(all_habitats))
  expect_equal(ncol(result$ns_provision_per_uniot_scores), length(all_services))
  expect_equal(rownames(result$ns_provision_per_uniot_scores), all_habitats)
  expect_equal(colnames(result$ns_provision_per_uniot_scores), all_services)

  # 3. Check Custom Divisor Alignment
  expect_equal(nrow(result$ns_custom_divisor_matrix), length(all_habitats))
  expect_equal(ncol(result$ns_custom_divisor_matrix), length(all_services))
})

test_that("Condition Indicator data is consistent", {
  skip_if_not(file.exists(path), message = "Spreadsheet not found")

  result <- openNCAI:::import_ns_data(path = path)
  ci_ids <- result$ns_indicator_directory$ci_id

  # 1. Check CI Score Matrix dimensions
  expect_equal(nrow(result$ns_ci_scores), length(result$ns_year_list))
  expect_equal(ncol(result$ns_ci_scores), length(ci_ids))
  expect_equal(colnames(result$ns_ci_scores), ci_ids)

  # 2. Check CIRMs list
  expect_length(result$ns_ci_relevance_matrices, length(ci_ids))
  expect_equal(names(result$ns_ci_relevance_matrices), ci_ids)

  # Check structure of the first CIRM
  first_cirm <- result$ns_ci_relevance_matrices[[1]]
  expect_equal(nrow(first_cirm), length(unlist(result$ns_habitats_label_tree)))
  expect_equal(ncol(first_cirm), length(unlist(result$ns_es_label_tree)))
})

test_that("Importance scores map correctly to ES Types", {
  skip_if_not(file.exists(path), message = "Spreadsheet not found")

  result <- openNCAI:::import_ns_data(path = path)
  service_types <- names(result$ns_es_label_tree)

  # Between scores
  expect_equal(names(result$ns_between_importance_scores), service_types)

  # Within scores
  expect_equal(names(result$ns_within_importance_scores), service_types)

  # Check that each list in 'within' matches the number of services in that type
  for (type in service_types) {
    expect_length(
      result$ns_within_importance_scores[[type]],
      length(result$ns_es_label_tree[[type]])
    )
  }
})
