test_that("index_and_smooth calculates totals, indexing, and smoothing correctly", {
  # 1. Setup Mock Data
  m_list <- list(
    "2000" = matrix(50, 2, 1), # Total 100
    "2001" = matrix(60, 2, 1), # Total 120 (Index 120)
    "2002" = matrix(50, 2, 1), # Total 100 (Index 100)
    "2003" = matrix(70, 2, 1), # Total 140 (Index 140)
    "2004" = matrix(50, 2, 1), # Total 100 (Index 100)
    "2005" = matrix(80, 2, 1)  # Total 160 (Index 160)
  )

  # 2. Run Function
  res <- index_and_smooth(m_list)

  # 3. Robust Verifications
  # Helper to extract value by row name safely
  get_val <- function(df, row_nm, col_nm) {
    as.numeric(df[which(rownames(df) == row_nm), col_nm])
  }

  # Check Raw Totals
  expect_equal(get_val(res, "2000", "raw_total"), 100)
  expect_equal(get_val(res, "2001", "raw_total"), 120)

  # Check Raw Index (Base 2000 = 100)
  expect_equal(get_val(res, "2000", "raw_index"), 100)
  expect_equal(get_val(res, "2001", "raw_index"), 120)

  # Check Smoothing (2004 window: 100, 120, 100, 140, 100)
  # Weights: 0.2, 0.4, 0.6, 0.8, 1.0 (Sum = 3.0) -> (340 / 3)
  expect_equal(get_val(res, "2004", "smoothed_index"), 340/3)
})


test_that("index_and_smooth accepts a numeric year_one", {
  m_list <- list("2000" = matrix(10), "2001" = matrix(20))

  res <- suppressMessages(index_and_smooth(m_list, year_one = 2001))

  expect_equal(res["2001", "raw_index"], 100)
  expect_equal(res["2000", "raw_index"], 50)
})

test_that("index_and_smooth errors when year_one is not in matrix_list", {
  m_list <- list("2000" = matrix(10), "2001" = matrix(20))

  expect_error(index_and_smooth(m_list, year_one = "1999"),
               class = "openNCAI_unknown_year")
})

test_that("index_and_smooth errors when the year_one total is zero", {
  m_list <- list("2000" = matrix(0), "2001" = matrix(20))

  expect_error(index_and_smooth(m_list), class = "openNCAI_zero_base")
})
