test_that("calc_importance_weights scales correctly and applies labels", {
  # 1. Setup metadata
  es_tree <- list(
    provisioning = c("crops", "timber"),
    cultural = c("recreation")
  )

  # Provisioning (60%) is more important than Cultural (40%)
  b_scores <- data.frame(
    score = c(60, 40),
    row.names = c("provisioning", "cultural")
  )

  # Individual scores within categories
  w_list <- list(
    provisioning = data.frame(score = c(8, 2)), # 80/20 split of the 60%
    cultural = data.frame(score = c(10))        # 100% of the 40%
  )

  # 2. Execute
  res <- calc_importance_weights(
    between_scores = b_scores,
    within_scores_list = w_list,
    es_label_tree = es_tree
  )

  # 3. Verify Math
  # provisioning total (60) * (8/10) = 48
  expect_equal(res$provisioning["crops", "weight"], 48)
  # provisioning total (60) * (2/10) = 12
  expect_equal(res$provisioning["timber", "weight"], 12)
  # cultural total (40) * (10/10) = 40
  expect_equal(res$cultural["recreation", "weight"], 40)

  # 4. Verify total sum across all lists is 100
  total_weight <- sum(unlist(lapply(res, function(df) df$weight)))
  expect_equal(total_weight, 100)

  # 5. Verify Row Labels were applied from the tree
  expect_equal(rownames(res$provisioning), c("crops", "timber"))
})

test_that("calc_importance_weights handles all-zero categories gracefully", {
  es_tree <- list(a = c("h1", "h2"))
  b_scores <- data.frame(score = c(100), row.names = c("a"))

  # Within scores are all zero
  w_list <- list(a = data.frame(score = c(0, 0)))

  res <- calc_importance_weights(b_scores, "score", w_list, "score", es_tree)

  # Should return 0 rather than NaN
  expect_equal(res$a$weight, c(0, 0))
})

test_that("calc_importance_weights catches dimension mismatches", {
  es_tree <- list(prov = c("crops", "timber")) # Expects 2
  b_scores <- data.frame(score = c(100), row.names = c("prov"))

  # User only provides 1 score
  w_list_bad <- list(prov = data.frame(score = c(10)))

  expect_error(
    calc_importance_weights(b_scores, "score", w_list_bad, "score", es_tree),
    "does not match the number of labels in es_label_tree"
  )
})
