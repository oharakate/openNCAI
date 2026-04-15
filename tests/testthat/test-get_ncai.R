# --- 1. Mock data setup
years <- c("2020", "2021", "2022")
h_tree <- list(woodland = c("forest"), grassland = c("meadow"))
e_tree <- list(provisioning = c("food"), regulating = c("climate"))

hab_names <- unlist(h_tree, use.names = FALSE) # "forest", "meadow"
es_names  <- unlist(e_tree, use.names = FALSE) # "food", "climate"

# Build extent with explicit row names
mock_extent <- data.frame(
  `2020` = c(100, 200),
  `2021` = c(105, 195),
  `2022` = c(110, 190),
  row.names = hab_names, # ADD THIS
  check.names = FALSE
)

mock_esppu <- data.frame(
  food = c(5, 1),
  climate = c(3, 4),
  row.names = hab_names # ADD THIS
)
colnames(mock_esppu) <- es_names

mock_between <- list(provisioning = 0.6, regulating = 0.4)
mock_within <- list(
  provisioning = list(food = 1),
  regulating = list(climate = 1)
)

mock_ci_scores <- matrix(
  c(1.0, 1.0,  # 2020
    1.1, 0.9,  # 2021
    1.2, 0.8), # 2022
  nrow = 3, byrow = TRUE,
  dimnames = list(years, c("Ind1", "Ind2"))
)

# Force CIRMs to match the variables exactly
mock_cirms <- list(
  Ind1 = data.frame(matrix(1, nrow = 2, ncol = 2,
                           dimnames = list(hab_names, es_names))),
  Ind2 = data.frame(matrix(1, nrow = 2, ncol = 2,
                           dimnames = list(hab_names, es_names)))
)

mock_dir <- data.frame(
  ci_id = c("Ind1", "Ind2"),
  provisioning = c(1, 0),
  regulating = c(0, 1),
  used = "Yes",
  stringsAsFactors = FALSE
)

# --- 2. FORCE EXPLICIT NAMING ALIGNMENT (REFINED FIX) ---
# We force these into standard data frames to prevent Tibble-related name stripping
mock_extent <- as.data.frame(mock_extent)
rownames(mock_extent) <- hab_names

mock_esppu <- as.data.frame(mock_esppu)
rownames(mock_esppu) <- hab_names
colnames(mock_esppu) <- es_names

# Ensure CIRMs are Data Frames with matching names
mock_cirms <- lapply(mock_cirms, function(m) {
  df <- as.data.frame(m)
  rownames(df) <- hab_names
  colnames(df) <- es_names
  return(df)
})

# --- 3. THE TESTS ---

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
  # Redefine years inside the block to avoid scoping issues
  years <- c("2020", "2021", "2022")

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
  years <- c("2020", "2021", "2022")

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


# Setup mock data for breakdown functions
m_list <- list(
  "2000" = matrix(50, 2, 1), # Total 100
  "2001" = matrix(60, 2, 1), # Total 120 (Index 120)
  "2002" = matrix(50, 2, 1), # Total 100 (Index 100)
  "2003" = matrix(70, 2, 1), # Total 140 (Index 140)
  "2004" = matrix(50, 2, 1), # Total 100 (Index 100)
  "2005" = matrix(80, 2, 1)  # Total 160 (Index 160)
)

test_that("calc_ncai_by_st correctly filters columns before indexing", {
  m1 <- matrix(c(10, 20), nrow = 1, dimnames = list("hab1", c("serv1", "serv2")))
  m_list <- list("2000" = m1, "2001" = m1 * 1.1)

  tree <- list(group_a = "serv1", group_b = "serv2")
  res <- openNCAI:::calc_ncai_by_st(m_list, tree)

  # Group A (serv1 only)
  val_a <- res$group_a$raw_total[which(rownames(res$group_a) == "2000")]
  expect_equal(as.numeric(val_a), 10)

  # Group B (serv2 only)
  val_b <- res$group_b$raw_total[which(rownames(res$group_b) == "2000")]
  expect_equal(as.numeric(val_b), 20)
})

test_that("calc_ncai_by_bh correctly filters rows before indexing", {
  m1 <- matrix(c(10, 20), nrow = 2, dimnames = list(c("hab1", "hab2"), "serv1"))
  m_list <- list("2000" = m1, "2001" = m1 * 1.1)

  tree <- list(woodland = "hab1", grassland = "hab2")
  res <- openNCAI:::calc_ncai_by_bh(m_list, tree)

  # Woodland (hab1 only)
  val_w <- res$woodland$raw_total[which(rownames(res$woodland) == "2000")]
  expect_equal(as.numeric(val_w), 10)

  # Grassland (hab2 only)
  val_g <- res$grassland$raw_total[which(rownames(res$grassland) == "2000")]
  expect_equal(as.numeric(val_g), 20)
})
