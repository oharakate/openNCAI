test_that("calc_espb calculates potential correctly and applies labels", {
  # Setup Mock Data
  h_tree <- list(a = c("h1", "h2"), b = c("h3"))
  es_tree <- list(services = c("es1", "es2"))

  # 3 habitats, 2 years
  extent <- data.frame(
    `2026` = c(10, 20, 30),
    `2027` = c(15, 25, 35),
    check.names = FALSE
  )

  # 3 habitats, 2 services
  weights <- data.frame(
    s1 = c(1, 0.5, 0),
    s2 = c(0, 0.5, 1)
  )

  years <- c("2026", "2027")

  # Execute
  res <- calc_espb(extent, weights, years, h_tree, es_tree)

  # 1. Check Math (10 * 1, 10 * 0, 20 * 0.5, etc.)
  expect_equal(res["h1", "es1"], 10)
  expect_equal(res["h2", "es2"], 10)
  expect_equal(res["h3", "es2"], 30)

  # 2. Check Labels
  expect_equal(rownames(res), c("h1", "h2", "h3"))
  expect_equal(colnames(res), c("es1", "es2"))
  expect_s3_class(res, "data.frame")
})

test_that("calc_espb throws errors for dimension mismatches", {
  h_tree <- list(a = c("h1")) # 1 habitat
  es_tree <- list(s = c("s1"))
  years <- c("2026")

  # Extent has 2 rows (Mismatch with tree)
  extent_bad <- data.frame(`2026` = c(10, 20), check.names = FALSE)
  weights_good <- data.frame(s1 = c(1))

  expect_error(
    calc_espb(extent_bad, weights_good, years, h_tree, es_tree),
    "match rows in habitat_extent"
  )
})
