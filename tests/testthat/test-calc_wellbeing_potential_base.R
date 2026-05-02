test_that("calc_wellbeing_potential_base calculates proportions and weights correctly", {
  # 1. Setup dummy data
  es_potential_base <- data.frame(
    crops = c(10, 30),
    timber = c(5, 5)
  )
  # Ensure rownames are present for the internal arithmetic
  rownames(es_potential_base) <- c("1", "2")

  # Weights: crops = 80, timber = 20
  weights <- data.frame(crops = 80, timber = 20)

  # 2. Execute
  res <- openNCAI:::calc_wellbeing_potential_base(es_potential_base, weights)

  # 3. Verify Math
  expect_equal(res$crops, c(2000, 6000))
  expect_equal(res$timber, c(1000, 1000))
  expect_s3_class(res, "data.frame")
})

test_that("calc_wellbeing_potential_base handles zero-sum columns (no NaN)", {
  es_potential_base <- data.frame(service1 = c(0, 0), service2 = c(10, 10))
  rownames(es_potential_base) <- c("1", "2")
  weights <- data.frame(service1 = 50, service2 = 50)

  # Should not produce NaN; service1 should remain 0
  res <- openNCAI:::calc_wellbeing_potential_base(es_potential_base, weights)

  expect_equal(res$service1, c(0, 0))
  expect_false(any(is.nan(as.matrix(res))))
})

test_that("calc_wellbeing_potential_base applies labels correctly when trees are provided", {
  h_tree <- list(grassland = c("meadow", "pasture"))
  es_tree <- list(provisioning = c("crops", "timber"))
  hab_names <- unlist(h_tree, use.names = FALSE) # Length is 2

  # Ensure the data frames have exactly 2 rows to match hab_names
  es_potential_base <- data.frame(
    crops = c(1, 1),
    timber = c(2, 2),
    row.names = hab_names # Both rows named
  )

  # weights must be a data frame with matching dimensions
  weights <- data.frame(
    crops = c(10, 10),
    timber = c(10, 10),
    row.names = hab_names
  )

  res <- openNCAI:::calc_wellbeing_potential_base(es_potential_base, weights, h_tree, es_tree)
  expect_equal(rownames(res), hab_names)
})

test_that("calc_wellbeing_potential_base returns unlabelled data frame if trees are NULL", {
  es_potential_base <- data.frame(c1 = c(1, 1), c2 = c(1, 1))
  rownames(es_potential_base) <- c("1", "2")
  weights <- data.frame(c1 = 10, c2 = 10)

  res <- openNCAI:::calc_wellbeing_potential_base(es_potential_base, weights, habitats_label_tree = NULL)

  # Should have default row names ("1", "2") and original col names
  expect_equal(rownames(res), c("1", "2"))
  expect_equal(colnames(res), c("c1", "c2"))
})
