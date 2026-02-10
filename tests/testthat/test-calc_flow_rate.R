test_that("calc_flow_rate returns a list with correct length and names", {

  # 1. Setup minimal mock objects
  years <- c("2000", "2001")

  # Mock CIRM: 1 habitat (h1), 1 service (s1)
  mock_matrix <- matrix(1, nrow = 1, ncol = 1,
                        dimnames = list("h1", "s1"))
  mock_cirm_list <- list(ind1 = mock_matrix)

  # Mock Label Trees
  # IMPORTANT: The name "group_a" is what stack() turns into 'ind'
  mock_es_tree <- list(group_a = "s1")
  mock_hab_tree <- list(all_habs = "h1")

  # Mock Indicator Directory
  # FIX: Column MUST be 'ci_id' to match the function's match() call
  # FIX: Column MUST be 'group_a' to match the es_tree names
  mock_ind_dir <- data.frame(
    ci_id = "ind1",
    group_a = 1.0,
    stringsAsFactors = FALSE
  )
  rownames(mock_ind_dir) <- "ind1"

  # Mock Condition Scores
  mock_scores <- data.frame(
    ind1 = c(100, 110),
    row.names = years
  )


  # 2. Run the master function
  results <- calc_flow_rate(
    cirm_list = mock_cirm_list,
    indicator_directory = mock_ind_dir,
    es_label_tree = mock_es_tree,
    habitats_label_tree = mock_hab_tree,
    ci_score_matrix = mock_scores,
    year_list = years,
    tir_constant = 2
  )

  # 3. Verifications
  expect_type(results, "list")
  expect_length(results, 2)
  expect_named(results, years)

  # Verify math:
  # TIR = (Weight 1.0 * Relevance 1) + Constant 2 = 3
  # Year 2000 Flow: (Condition 100 + (2 * 100)) / 3 = 100
  # Year 2001 Flow: (Condition 110 + (2 * 100)) / 3 = 103.333
  expect_equal(as.numeric(results[["2000"]][1,1]), 100)
  expect_equal(as.numeric(results[["2001"]][1,1]), 310/3)
})
