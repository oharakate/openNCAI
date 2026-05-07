test_that("calc_es_potential_base calculates potential correctly and applies labels", {
  # Setup Mock Data
  # h_tree total leaf nodes = 2 ("h1", "h2")
  h_tree <- list(a = c("h1"), b = c("h2"))
  # es_tree total leaf nodes = 2 ("es1", "es2")
  es_tree <- list(services = c("es1", "es2"))

  years <- c("2026", "2027")

  # 1. Extent: 2 rows (h1, h2), columns = years
  # We use check.names = FALSE to allow numeric column names
  extent <- data.frame(
    `2026` = c(10, 20),
    `2027` = c(15, 25),
    row.names = unlist(h_tree, use.names = FALSE),
    check.names = FALSE
  )

  # 2. Weights: Rows = habitats (2), Cols = services (2)
  # colnames must match the unlisted es_tree values
  weights <- data.frame(
    es1 = c(1, 0.5), # Weights for h1, h2
    es2 = c(0, 0.5),
    row.names = unlist(h_tree, use.names = FALSE)
  )

  # Execute
  # Assuming year_one defaults to the first year in 'years' if not provided
  res <- calc_es_potential_base(
    habitat_extent = extent,
    provision_per_unit_weights = weights,
    year_list = years,
    year_one = "2026",
    habitats_label_tree = h_tree,
    es_label_tree = es_tree
  )

  # --- VERIFICATION ---

  # 1. Check Math for year_one (2026)
  # h1: 10 (extent) * 1 (weight) = 10
  expect_equal(res["h1", "es1"], 10)
  # h2: 20 (extent) * 0.5 (weight) = 10
  expect_equal(res["h2", "es1"], 10)
  expect_equal(res["h2", "es2"], 10)

  # 2. Check Labels
  expect_equal(rownames(res), c("h1", "h2"))
  expect_equal(colnames(res), c("es1", "es2"))
  expect_s3_class(res, "data.frame")
})

test_that("calc_es_potential_base throws errors for dimension mismatches", {
  h_tree <- list(a = c("h1"))
  es_tree <- list(s = c("s1"))
  years <- c("2026")

  # Extent has 2 rows (Mismatch with h_tree which has 1)
  extent_bad <- data.frame(`2026` = c(10, 20), check.names = FALSE)
  weights_good <- data.frame(s1 = c(1))

  # Name your arguments to ensure h_tree isn't treated as a year
  expect_error(
    calc_es_potential_base(
      habitat_extent = extent_bad,
      provision_per_unit_weights = weights_good,
      year_list = years,
      year_one = "2026", # Explicitly set this
      habitats_label_tree = h_tree,
      es_label_tree = es_tree
    ),
    "match rows in habitat_extent"
  )
})
