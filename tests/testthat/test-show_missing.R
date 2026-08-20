test_that("show_missing on a complete data frame just messages and returns invisibly", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_message(res <- show_missing(df), "Complete data supplied")
  expect_null(res)
})

test_that("show_missing on a data frame with missingness returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
  p <- suppressMessages(show_missing(df))
  expect_s3_class(p, "ggplot")
})

test_that("show_missing uses label_col instead of rownames when given one", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ci_id = c("alpha", "beta"), score = c(1, NA))
  p <- suppressMessages(show_missing(df, label_col = "ci_id"))
  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$label), sort(c("alpha", "beta")))
})

test_that("show_missing on a complete tree just messages and returns invisibly", {
  tree <- list(broad_a = c("d1", "d2"), broad_b = c("d3"))
  expect_message(res <- show_missing(tree), "Complete data supplied")
  expect_null(res)
})

test_that("show_missing on a tree lists which names/values are missing", {
  tree <- list(broad_a = c("d1", NA), broad_b = c("d3"))
  names(tree)[2] <- ""
  expect_message(res <- show_missing(tree), "name is blank")
  expect_message(show_missing(tree), "value missing")
  expect_null(res)
})

test_that("show_missing on a vector lists which elements are missing", {
  x <- c(a = "2020", b = NA, c = "2022")
  expect_message(res <- show_missing(x), "\\[b\\]: value missing")
  expect_null(res)
})

test_that("show_missing errors on an object it doesn't recognise", {
  expect_error(show_missing(quote(x)), "doesn't know how to handle")
})
