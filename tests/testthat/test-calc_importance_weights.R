test_that("calc_importance_weights correctly matches by name regardless of list order", {
  # 1. Define Hierarchy
  es_tree <- list(
    provisioning = c("crops", "timber"),
    regulating = c("carbon")
  )

  # 2. Setup scores in "Wrong" order (Regulating first)
  # This tests if the function correctly pulls 'provisioning' first as per the tree
  b_scores <- list(regulating = 25, provisioning = 75)

  w_scores <- list(
    regulating = list(carbon = 10),
    provisioning = list(timber = 5, crops = 5) # timber first here, tree says crops first
  )

  res <- openNCAI:::calc_importance_weights(b_scores, w_scores, es_tree)

  # 3. Verify Order and Math
  # Total b_score = 100. Provisioning = 75%, Regulating = 25%
  # Provisioning within: crops = 5/10 (0.5), timber = 5/10 (0.5)
  # Final crops: 0.5 * 75 = 37.5
  # Final timber: 0.5 * 75 = 37.5
  # Final carbon: 1.0 * 25 = 25.0

  expect_equal(colnames(res), c("crops", "timber", "carbon"))
  expect_equal(as.numeric(res[1,]), c(37.5, 37.5, 25.0))
})

test_that("calc_importance_weights throws error for missing broad categories", {
  es_tree <- list(prov = "crops", reg = "carbon")
  # Missing 'reg' in between_scores
  b_scores <- list(prov = 100)
  w_scores <- list(prov = list(crops = 1), reg = list(carbon = 1))

  expect_error(openNCAI:::calc_importance_weights(b_scores, w_scores, es_tree),
               "between_scores is missing required categories")
})

test_that("calc_importance_weights throws error for missing specific service labels", {
  es_tree <- list(prov = c("crops", "timber"))
  b_scores <- list(prov = 100)
  # Missing 'timber' in within_scores
  w_scores <- list(prov = list(crops = 10))

  expect_error(openNCAI:::calc_importance_weights(b_scores, w_scores, es_tree),
               "Specific service labels for 'prov' were not found")
})

test_that("calc_importance_weights handles all-zero categories", {
  es_tree <- list(prov = c("crops", "timber"))
  b_scores <- list(prov = 100)
  # Total sum for prov is 0
  w_scores <- list(prov = list(crops = 0, timber = 0))

  res <- openNCAI:::calc_importance_weights(b_scores, w_scores, es_tree)
  expect_equal(as.numeric(res[1,]), c(0, 0))
})
