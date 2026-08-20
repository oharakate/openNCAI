test_that("check_missing passes silently on complete inputs of every shape", {
  inputs <- list(
    a_df = data.frame(x = 1:3, y = 4:6),
    a_tree = list(broad_a = c("d1", "d2"), broad_b = c("d3")),
    a_vector = c("2020", "2021", "2022"),
    a_list_of_df = list(
      ci_one = data.frame(x = 1:2, y = 3:4),
      ci_two = data.frame(x = 5:6, y = 7:8)
    )
  )

  expect_message(res <- check_missing(inputs), "Complete\\.")
  expect_true(res)
})

test_that("check_missing reports and stops on a data frame with missing values", {
  df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
  rownames(df) <- c("row1", "row2", "row3")

  expect_error(check_missing(list(my_df = df)), "- my_df")
  expect_message(
    tryCatch(check_missing(list(my_df = df)), error = function(e) invisible(NULL)),
    "row2.*missing|row3.*missing"
  )
})

test_that("check_missing treats a blank list name as missing, alongside blank values", {
  tree <- list(broad_a = c("d1", "d2"), broad_b = c("d3"))
  names(tree)[2] <- ""

  expect_error(check_missing(list(habitats_label_tree = tree)), "habitats_label_tree")
  expect_message(
    tryCatch(check_missing(list(habitats_label_tree = tree)), error = function(e) invisible(NULL)),
    "name is blank"
  )
})

test_that("check_missing gives ci_relevance_matrices its own header and per-element sub-lines", {
  cirm <- list(
    ci_complete = data.frame(x = 1:2),
    ci_broken = data.frame(x = c(1, NA))
  )

  expect_error(check_missing(list(ci_relevance_matrices = cirm)), "ci_relevance_matrices\\$ci_broken")
  expect_message(
    tryCatch(check_missing(list(ci_relevance_matrices = cirm)), error = function(e) invisible(NULL)),
    "ci_relevance_matrices:"
  )
})

test_that("check_missing requires a fully named list", {
  expect_error(check_missing(list(1, 2)), "fully named list")
  expect_error(check_missing(data.frame(x = 1)), "fully named list")
})
