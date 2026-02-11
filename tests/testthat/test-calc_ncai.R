test_that("calc_ncai calculates totals, indexing, and smoothing correctly", {
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
  res <- openNCAI:::calc_ncai(m_list)

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

test_that("calc_ncai_by_st correctly filters columns before indexing", {
  m1 <- matrix(c(10, 20), nrow = 1, dimnames = list("hab1", c("serv1", "serv2")))
  m_list <- list("2000" = m1, "2001" = m1 * 1.1)

  tree <- list(group_a = "serv1", group_b = "serv2")
  res <- openNCAI:::calc_ncai_by_st(m_list, tree)

  # Group A (serv1 only)
  val_a <- res$group_a$raw_total[which(rownames(res$group_a) == "2000")]
  expect_equal(as.numeric(val_a), 10)

  # Group B (serv2 only)
  val_b <- res$group_b$raw_total[which(rownames(res$group_b) == "2000")]
  expect_equal(as.numeric(val_b), 20)
})

test_that("calc_ncai_by_bh correctly filters rows before indexing", {
  m1 <- matrix(c(10, 20), nrow = 2, dimnames = list(c("hab1", "hab2"), "serv1"))
  m_list <- list("2000" = m1, "2001" = m1 * 1.1)

  tree <- list(woodland = "hab1", grassland = "hab2")
  res <- openNCAI:::calc_ncai_by_bh(m_list, tree)

  # Woodland (hab1 only)
  val_w <- res$woodland$raw_total[which(rownames(res$woodland) == "2000")]
  expect_equal(as.numeric(val_w), 10)

  # Grassland (hab2 only)
  val_g <- res$grassland$raw_total[which(rownames(res$grassland) == "2000")]
  expect_equal(as.numeric(val_g), 20)
})
