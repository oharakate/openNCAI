# --- 1. MOCK DATA SETUP ---
years <- c("2020", "2021", "2022")
h_tree <- list(woodland = c("forest"), grassland = c("meadow"))
e_tree <- list(provisioning = c("food"), regulating = c("climate"))

mock_extent <- data.frame(
  `2020` = c(100, 200),
  `2021` = c(105, 195),
  `2022` = c(110, 190),
  check.names = FALSE,
  row.names = c("forest", "meadow")
)

mock_esppu <- data.frame(
  food = c(5, 1),
  climate = c(3, 4),
  row.names = c("forest", "meadow")
)

mock_between <- c(provisioning = 0.6, regulating = 0.4)
mock_within <- list(provisioning = c(food = 1), regulating = c(climate = 1))

mock_ci_scores <- matrix(
  c(1.0, 1.0,  # 2020 (Ind1, Ind2)
    1.1, 0.9,  # 2021
    1.2, 0.8), # 2022
  nrow = 3, byrow = TRUE,
  dimnames = list(years, c("Ind1", "Ind2"))
)

# CIRMS must match leaf nodes (food/climate) for labeling
mock_cirms <- list(
  Ind1 = matrix(1, nrow = 2, ncol = 2,
                dimnames = list(c("forest", "meadow"), c("food", "climate"))),
  Ind2 = matrix(1, nrow = 2, ncol = 2,
                dimnames = list(c("forest", "meadow"), c("food", "climate")))
)

# Columns match names(e_tree) which is what stack() produces for 'ind'
mock_dir <- data.frame(
  ci_id = c("Ind1", "Ind2"),
  provisioning = c(1, 0), # Weights for the 'provisioning' group
  regulating = c(0, 1),   # Weights for the 'regulating' group
  stringsAsFactors = FALSE
)

# --- 2. THE TESTS ---

test_that("get_ncai core logic works with default return", {
  res <- openNCAI::get_ncai(
    habitat_extent = mock_extent,
    ci_scores = mock_ci_scores,
    habitats_label_tree = h_tree,
    es_label_tree = e_tree,
    year_list = years,
    esppu_scores = mock_esppu,
    esppu_divisor = 5,
    between_importance_scores = mock_between,
    within_importance_scores = mock_within,
    ci_relevance_matrices = mock_cirms,
    indicator_directory = mock_dir
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res["2020", "raw_index"], 100)
})

test_that("get_ncai correctly handles custom year_one", {
  expect_message(
    res <- openNCAI::get_ncai(
      habitat_extent = mock_extent,
      ci_scores = mock_ci_scores,
      habitats_label_tree = h_tree,
      es_label_tree = e_tree,
      year_list = years,
      year_one = "2021",
      esppu_scores = mock_esppu,
      esppu_divisor = 5,
      between_importance_scores = mock_between,
      within_importance_scores = mock_within,
      ci_relevance_matrices = mock_cirms,
      indicator_directory = mock_dir
    ),
    "Note: Smoothed index at baseline year"
  )
  expect_equal(res["2021", "raw_index"], 100)
})

test_that("get_ncai returns the full results list when requested", {
  res_all <- openNCAI::get_ncai(
    habitat_extent = mock_extent,
    ci_scores = mock_ci_scores,
    habitats_label_tree = h_tree,
    es_label_tree = e_tree,
    year_list = years,
    esppu_scores = mock_esppu,
    esppu_divisor = 5,
    between_importance_scores = mock_between,
    within_importance_scores = mock_within,
    ci_relevance_matrices = mock_cirms,
    indicator_directory = mock_dir,
    return = "everything"
  )
  expect_type(res_all, "list")
  expect_named(res_all, c("espb", "wellbeing_base", "yearly_flow_matrices",
                          "yearly_asset_matrices", "overall_index",
                          "index_by_st", "index_by_bh"))
})
