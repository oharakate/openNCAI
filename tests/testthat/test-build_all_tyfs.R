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
