test_that("calc_weighted_habitat_extent handles dual output modes correctly", {
  # 1. Setup minimal dummy data
  hab_extent <- data.frame(
    `2020` = c(10, 20),
    `2021` = c(12, 18),
    row.names = c("Forest", "Grassland"),
    check.names = FALSE
  )

  weights <- data.frame(
    Service_A = c(1, 0),
    Service_B = c(0.5, 2),
    row.names = c("Forest", "Grassland")
  )

  # 2. Test as_matrices = TRUE (returns list of matrices)
  result_mats <- calc_weighted_habitat_extent(
    habitat_extent = hab_extent,
    year_one = 2020,
    weight_matrix = weights,
    as_matrices = TRUE
  )

  expect_type(result_mats, "list")
  expect_named(result_mats, c("2020", "2021"))
  expect_true(is.matrix(result_mats[[1]]) || is.data.frame(result_mats[[1]]))

  # 3. Test as_matrices = FALSE (returns data frame)
  result_df <- calc_weighted_habitat_extent(
    habitat_extent = hab_extent,
    year_one = 2020,
    weight_matrix = weights,
    as_matrices = FALSE
  )

  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 2)
  expect_equal(result_df["2020", "raw_index"], 100)
  expect_true(all(c("raw_total", "raw_index", "smoothed_index") %in% colnames(result_df)))
})

test_that("get_yearly_potential_provision wraps worker correctly", {
  # Mock data
  hab_extent <- data.frame(`2020` = 10, row.names = "Forest", check.names = FALSE)
  es_potential_base <- data.frame(S1 = 5, row.names = "Forest")

  # We test that it returns the expected structure
  res <- get_yearly_potential_provision(hab_extent, 2020, es_potential_base)

  expect_s3_class(res, "data.frame")
  expect_equal(res["2020", "raw_total"], 500) # 10 extent * 5 weight
})

test_that("get_yearly_potential_wellbeing wraps worker correctly", {
  # Mock data
  hab_extent <- data.frame(`2020` = 10, row.names = "Forest", check.names = FALSE)
  wb_base <- data.frame(W1 = 2, row.names = "Forest")

  res <- get_yearly_potential_wellbeing(hab_extent, 2020, wb_base)

  expect_s3_class(res, "data.frame")
  expect_equal(res["2020", "raw_total"], 200) # 10 extent * 2 weight
})

test_that("potential functions handle numeric and character years interchangeably", {
  hab_extent <- data.frame(`2020` = 10, `2021` = 10, row.names = "Forest", check.names = FALSE)
  es_potential_base <- data.frame(S1 = 5, row.names = "Forest")

  # Test with numeric 2020
  res_num <- get_yearly_potential_provision(hab_extent, 2020, es_potential_base)
  # Test with character "2020"
  res_chr <- get_yearly_potential_provision(hab_extent, "2020", es_potential_base)

  expect_equal(res_num$raw_index, res_chr$raw_index)
})
