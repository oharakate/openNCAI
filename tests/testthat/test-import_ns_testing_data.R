path <- testthat::test_path("../../data-raw/ncai_corrected.xlsx")

test_that("import_ns_testing_data returns expected structure and names", {
  skip_if_not(file.exists(path), "Spreadsheet not found")

  res <- import_ns_testing_data(
    path = path,
    habitats_label_tree = ns_habitats_label_tree,
    es_label_tree = ns_es_label_tree,
    year_list = ns_year_list # Pass from helper.R
  )

  expect_named(res, c("ref_es_potential_base", "ref_wellbeing_potential_base", "ref_total_indicator_relevances",
                      "ref_all_year_sheets", "ref_index_breakdowns"))
})

test_that("read_the_indices correctly formats output", {
  skip_if_not(file.exists(path), "Spreadsheet not found")

  indices <- read_the_indices(
    indices_range = "B2:D24",
    path = path,
    sheet = 73,
    year_list = ns_year_list
  )

  expect_named(indices, c("raw_total", "raw_index", "smoothed_index"))
  # Verify rows match the length of our mock ns_year_list (23)
  expect_equal(nrow(indices), 23)
})

test_that("read_ns_year_sheet handles NA values", {
  skip_if_not(file.exists(path), "Spreadsheet not found")

  sheet_data <- read_ns_year_sheet(
    sheet = 50,
    path = path,
    es_label_tree = ns_es_label_tree,
    habitats_label_tree = ns_habitats_label_tree
  )

  expect_false(any(is.na(sheet_data)))
})
