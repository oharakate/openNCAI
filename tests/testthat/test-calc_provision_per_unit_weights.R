test_that("calc_provision_per_unit_weights handles missing divisors", {
  df <- data.frame(es1 = c(10, 20), es2 = c(30, 40))
  rownames(df) <- c("h1", "h2")

  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1", "es2"))

  # Explicitly passing NULL should trigger our custom error message
  expect_error(
    openNCAI:::calc_provision_per_unit_weights(
      df,
      divisor = NULL,
      custom_divisor_matrix = NULL,
      habitats_label_tree = h_tree,
      es_label_tree = es_tree
    ),
    "You must provide either a 'divisor' or a 'custom_divisor_matrix'"
  )
})

test_that("calc_provision_per_unit_weights works with a single divisor", {
  df <- data.frame(a = c(10, 20), b = c(30, 40))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1", "es2"))

  # FIX: Align names
  rownames(df) <- c("h1", "h2")
  colnames(df) <- c("es1", "es2")

  res <- openNCAI:::calc_provision_per_unit_weights(df,
                                           divisor = 10,
                                           habitats_label_tree = h_tree,
                                           es_label_tree = es_tree)

  expect_equal(res$es1, c(1, 2))
  expect_equal(res$es2, c(3, 4))
  expect_equal(rownames(res), c("h1", "h2"))
})

test_that("calc_provision_per_unit_weights works with custom matrix", {
  df <- data.frame(a = c(10, 20))
  custom <- data.frame(a = c(2, 5))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1"))

  # FIX: Align names for both the data and the divisor matrix
  rownames(df) <- rownames(custom) <- c("h1", "h2")
  colnames(df) <- colnames(custom) <- c("es1")

  res <- openNCAI:::calc_provision_per_unit_weights(df,
                                           custom_divisor_matrix = custom,
                                           habitats_label_tree =  h_tree,
                                           es_label_tree = es_tree)

  expect_equal(res$es1, c(5, 4))
})

test_that("calc_provision_per_unit_weights catches dimension mismatches", {
  df <- data.frame(a = c(10, 20))
  wrong_dim <- data.frame(a = c(2, 5, 10))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1"))

  rownames(df) <- c("h1", "h2")
  colnames(df) <- "es1"

  expect_error(
    openNCAI:::calc_provision_per_unit_weights(
      df,
      custom_divisor_matrix = wrong_dim,
      habitats_label_tree = h_tree,
      es_label_tree = es_tree
    ),
    # UPDATED: Match the exact error string from the function
    "Dimensions of 'provision_per_unit_scores' and 'custom_divisor_matrix' must match"
  )
})
