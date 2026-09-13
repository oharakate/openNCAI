# --- Helper Function Tests ---

test_that("get_yearly_condition calculates index relative to base year", {
  # Setup mock raw condition scores
  raw_cis <- data.frame(
    ind1 = c(50, 60),
    ind2 = c(10, 5),
    row.names = c("2000", "2001")
  )
  years <- c("2000", "2001")

  # Test ind1 for 2001: (60 / 50) * 100 = 120
  res1 <- get_yearly_condition(raw_cis, "2001", 1, years)
  expect_equal(res1, 120)

  # Test ind2 for 2001: (5 / 10) * 100 = 50
  res2 <- get_yearly_condition(raw_cis, "2001", 2, years)
  expect_equal(res2, 50)
})

test_that("build_tyf returns 100 when all condition scores are 100", {
  # Setup Data
  total_indicator_relevances_mat <- matrix(c(0.5, 1.0, 1.0, 0.5), nrow = 2)
  ywccm_list <- list(
    ind1 = matrix(c(50, 100, 100, 50), nrow = 2)
  )
  constant <- 2
  total_total_indicator_relevances <- total_indicator_relevances_mat + constant

  # Run Function
  res <- build_tyf(ywccm_list, total_total_indicator_relevances, constant)

  # Math: (50 + (100 * 2)) / (0.5 + 2) = 100
  expect_equal(res[1, 1], 100)
  expect_equal(res[2, 2], 100)
})

test_that("build_tyf sets zero-relevance cells to 100 even when constant is 0", {
  # Cell [1,1] has zero raw relevance (no indicator relevant there), so its
  # weighted condition contribution is also zero, by construction. Cell
  # [2,2] has real, non-zero relevance and a real signal, and should be
  # left untouched by the zero-relevance handling.
  raw_relevance <- matrix(c(0, 1, 1, 0.5), nrow = 2)
  constant <- 0
  total_indicator_relevances <- raw_relevance + constant
  ywccm_list <- list(ind1 = matrix(c(0, 100, 100, 25), nrow = 2))

  res <- build_tyf(ywccm_list, total_indicator_relevances, constant)

  expect_equal(res[1, 1], 100)
  # Math: (25 + (100 * 0)) / (0.5 + 0) = 50
  expect_equal(res[2, 2], 50)
})

test_that("build_tyf's zero-relevance handling matches existing behaviour for constant > 0", {
  raw_relevance <- matrix(0, nrow = 1, ncol = 1)
  constant <- 2
  total_indicator_relevances <- raw_relevance + constant
  ywccm_list <- list(ind1 = matrix(0, nrow = 1, ncol = 1))

  res <- build_tyf(ywccm_list, total_indicator_relevances, constant)

  # Math: (0 + (100 * 2)) / (0 + 2) = 100, same result the division alone
  # already produced before the explicit zero-relevance handling was added.
  expect_equal(res[1, 1], 100)
})

# --- Orchestrator Function Tests ---

test_that("build_all_ywccms multiplies scalars across list of matrices", {
  raw_cis <- data.frame(
    ind1 = c(100, 110),
    row.names = c("2000", "2001")
  )
  years <- c("2000", "2001")
  mat1 <- matrix(1, nrow = 2, ncol = 2,
                 dimnames = list(c("h1", "h2"), c("s1", "s2")))
  ciwms <- list(ind1 = mat1)

  # Condition index for ind1 in 2001 is 110
  results <- build_all_ywccms(raw_cis, "2001", years, ciwms)

  expect_equal(results$ind1[1,1], 110)
  expect_equal(results$ind1[2,2], 110)
  expect_named(results, "ind1")
  expect_true(is.matrix(results$ind1))
})

test_that("build_all_ywccms handles character and numeric year inputs", {
  raw_cis <- data.frame(ind1 = c(100, 100), row.names = c("2000", "2001"))
  years <- c(2000, 2001)
  ciwms <- list(ind1 = matrix(1, 1, 1))

  expect_no_error(build_all_ywccms(raw_cis, 2000, years, ciwms))
})

test_that("build_all_tyfs correctly iterates and names list by year", {
  years <- c(2000, 2001)
  raw_cis <- data.frame(ind1 = c(100, 110), row.names = c("2000", "2001"))
  ciwms <- list(ind1 = matrix(1, 1, 1))
  total_indicator_relevances <- matrix(3, 1, 1) # (1 + constant 2)
  constant <- 2

  results <- build_all_tyfs(raw_cis, years, ciwms, total_indicator_relevances, constant)

  expect_length(results, 2)
  expect_named(results, c("2000", "2001"))
  expect_equal(results[["2000"]][1,1], 100)
  expect_equal(results[["2001"]][1,1], 310/3)
})
