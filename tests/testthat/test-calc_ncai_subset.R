# Two habitats in one group, two services in one group, plus one of each
# outside the groups
subset_mats <- list(
  "2000" = data.frame(s1 = c(1, 2, 3), s2 = c(4, 5, 6), s3 = c(7, 8, 9),
                      row.names = c("h1", "h2", "h3")),
  "2001" = data.frame(s1 = c(2, 4, 6), s2 = c(8, 10, 12), s3 = c(14, 16, 18),
                      row.names = c("h1", "h2", "h3"))
)
hab_tree <- list(woods = c("h1", "h2"), other = "h3")
es_tree <- list(cultural = c("s1", "s2"), other = "s3")

test_that("calc_ncai_subset indexes the intersection of habitats and services", {
  res <- calc_ncai_subset(subset_mats, habitats = c("h1", "h2"),
                          services = c("s1", "s2"))

  expect_equal(res["2000", "raw_total"], 1 + 2 + 4 + 5)
  expect_equal(res["2001", "raw_index"], 200)
})

test_that("calc_ncai_subset expands group names from the label trees", {
  by_group <- calc_ncai_subset(subset_mats, habitats = "woods",
                               services = "cultural",
                               habitats_label_tree = hab_tree,
                               es_label_tree = es_tree)
  by_label <- calc_ncai_subset(subset_mats, habitats = c("h1", "h2"),
                               services = c("s1", "s2"))

  expect_equal(by_group, by_label)
})

test_that("calc_ncai_subset counts a label selected more than once only once", {
  res <- calc_ncai_subset(subset_mats, habitats = c("woods", "h1"),
                          services = "s1", habitats_label_tree = hab_tree)

  expect_equal(res["2000", "raw_total"], 1 + 2)
})

test_that("calc_ncai_subset defaults to all habitats and services", {
  expect_equal(calc_ncai_subset(subset_mats), index_and_smooth(subset_mats))
})

test_that("calc_ncai_subset errors on unknown labels instead of dropping them", {
  expect_error(
    calc_ncai_subset(subset_mats, habitats = c("h1", "h9")),
    class = "openNCAI_unknown_label"
  )
  expect_error(
    calc_ncai_subset(subset_mats, services = "s9"),
    class = "openNCAI_unknown_label"
  )
  # Group names are unknown unless their tree is supplied
  expect_error(
    calc_ncai_subset(subset_mats, habitats = "woods"),
    class = "openNCAI_unknown_label"
  )
})

test_that("calc_ncai_subset indexes on a custom year_one", {
  res <- suppressMessages(calc_ncai_subset(subset_mats, habitats = "h1",
                                           year_one = "2001"))

  expect_equal(res["2001", "raw_index"], 100)
  expect_equal(res["2000", "raw_index"], 50)
})
