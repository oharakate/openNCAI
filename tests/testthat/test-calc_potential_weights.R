test_that("calc_potential_weights handles missing divisors", {
  df <- data.frame(a = c(10, 20), b = c(30, 40))
  # Dummy trees so the function doesn't fail on missing arguments
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1", "es2"))

  expect_error(
    openNCAI:::calc_potential_weights(df, divisor = NULL, custom_divisor_matrix = NULL, h_tree, es_tree),
    "You must provide either a 'divisor' or a 'custom_divisor_matrix'"
  )
})

test_that("calc_potential_weights works with a single divisor", {
  df <- data.frame(a = c(10, 20), b = c(30, 40))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1", "es2"))

  # Pass trees here
  res <- openNCAI:::calc_potential_weights(df,
                                 divisor = 10,
                                 habitats_label_tree = h_tree,
                                 es_label_tree = es_tree)

  expect_equal(res$es1, c(1, 2))
  expect_equal(res$es2, c(3, 4))

  expect_equal(rownames(res), c("h1", "h2"))
})

test_that("calc_potential_weights works with custom matrix", {
  df <- data.frame(a = c(10, 20))
  custom <- data.frame(a = c(2, 5))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1"))

  # Pass trees here
  res <- openNCAI:::calc_potential_weights(df,
                                 custom_divisor_matrix = custom,
                                 habitats_label_tree =  h_tree,
                                 es_label_tree = es_tree)

  expect_equal(res$es1, c(5, 4))
})

test_that("calc_potential_weights catches dimension mismatches", {
  df <- data.frame(a = c(10, 20))
  wrong_dim <- data.frame(a = c(2, 5, 10))
  h_tree <- list(cat = c("h1", "h2"))
  es_tree <- list(cat = c("es1"))

  expect_error(
    openNCAI:::calc_potential_weights(df,
                            custom_divisor_matrix = wrong_dim,
                            habitats_label_tree = h_tree,
                            es_label_tree = es_tree),
    "Dimensions of esppu and custom_divisor_matrix must match"
  )
})
