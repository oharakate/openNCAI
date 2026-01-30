library(testthat)

test_that("calc_wellbeing_base calculates proportions and weights correctly", {
  # 1. Setup dummy data
  # Habitat A (10) + Habitat B (30) = 40 total potential for crops
  # Habitat A (5)  + Habitat B (5)  = 10 total potential for timber
  espb <- data.frame(
    crops = c(10, 30),
    timber = c(5, 5)
  )

  # Weights: crops = 80, timber = 20
  weights <- data.frame(crops = 80, timber = 20)

  # 2. Execute
  res <- calc_wellbeing_base(espb, weights)

  # 3. Verify Math
  # Crops: Hab A prop = 10/40 (0.25). Weighted = 0.25 * 80 * 100 = 2000
  # Crops: Hab B prop = 30/40 (0.75). Weighted = 0.75 * 80 * 100 = 6000
  # Timber: Hab A prop = 5/10 (0.5). Weighted = 0.5 * 20 * 100 = 1000
  # Timber: Hab B prop = 5/10 (0.5). Weighted = 0.5 * 20 * 100 = 1000

  expect_equal(res$crops, c(2000, 6000))
  expect_equal(res$timber, c(1000, 1000))
  expect_s3_class(res, "data.frame")
})

test_that("calc_wellbeing_base handles zero-sum columns (no NaN)", {
  espb <- data.frame(service1 = c(0, 0), service2 = c(10, 10))
  weights <- data.frame(service1 = 50, service2 = 50)

  # Should not produce NaN; service1 should remain 0
  res <- calc_wellbeing_base(espb, weights)

  expect_equal(res$service1, c(0, 0))
  expect_false(any(is.nan(as.matrix(res))))
})

test_that("calc_wellbeing_base applies labels correctly when trees are provided", {
  espb <- data.frame(c1 = c(1, 1), c2 = c(2, 2))
  weights <- data.frame(c1 = 10, c2 = 10)

  h_tree <- list(grassland = c("meadow", "pasture"))
  es_tree <- list(provisioning = c("crops", "timber"))

  res <- calc_wellbeing_base(espb, weights, h_tree, es_tree)

  # Check labels
  expect_equal(rownames(res), c("meadow", "pasture"))
  expect_equal(colnames(res), c("crops", "timber"))
})

test_that("calc_wellbeing_base returns unlabelled data frame if trees are NULL", {
  espb <- data.frame(c1 = c(1, 1), c2 = c(1, 1))
  weights <- data.frame(c1 = 10, c2 = 10)

  res <- calc_wellbeing_base(espb, weights, habitats_label_tree = NULL)

  # Should have default row names ("1", "2") and original col names
  expect_equal(rownames(res), c("1", "2"))
  expect_equal(colnames(res), c("c1", "c2"))
})
