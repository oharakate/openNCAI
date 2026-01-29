test_that("bind_importance_weights correctly flattens and labels", {
  # Setup metadata
  es_tree <- list(
    prov = c("crops", "timber"),
    cult = c("rec")
  )

  # Setup weights in REVERSE order to test alignment logic
  w_list <- list(
    cult = data.frame(weight = 40),
    prov = data.frame(weight = c(48, 12))
  )

  # Execute
  res <- bind_importance_weights(w_list, es_tree)

  # Verify structure
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 3)

  # Verify slignment (prov should come first because it's first in the tree)
  # Weights should be: crops(48), timber(12), rec(40)
  expect_equal(colnames(res), c("crops", "timber", "rec"))
  expect_equal(as.numeric(res[1, ]), c(48, 12, 40))
})

test_that("bind_importance_weights catches missing weight columns", {
  es_tree <- list(prov = c("crops"))
  # Using 'score' instead of 'weight'
  w_list <- list(prov = data.frame(score = 100))

  expect_error(
    bind_importance_weights(w_list, es_tree),
    "missing the required 'weight' column"
  )
})

test_that("bind_importance_weights catches missing categories", {
  es_tree <- list(prov = c("crops"), cult = c("rec"))
  # User forgot the 'cult' category
  w_list <- list(prov = data.frame(weight = 100))

  expect_error(
    bind_importance_weights(w_list, es_tree),
    "from the tree is missing from your weight list"
  )
})

test_that("bind_importance_weights catches length mismatches", {
  es_tree <- list(prov = c("crops", "timber")) # Expects 2 labels

  # User provides 3 weights for 2 labels
  w_list <- list(prov = data.frame(weight = c(10, 20, 30)))

  # Use a partial string match to be safe
  expect_error(
    bind_importance_weights(w_list, es_tree),
    "Weight count.*does not match.*labels in tree"
  )
})
