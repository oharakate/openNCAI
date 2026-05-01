test_that("Template round-trip works correctly", {
  # 1. Setup Toy Data
  toy_hab_tree <- list(woodland = c("oak", "pine"), coastal = c("saltmarsh"))
  toy_es_tree <- list(provisioning = c("crops"), cultural = c("recreation"))
  toy_cis <- c("Indicator A", "Indicator B")
  toy_years <- 2020:2022

  # 2. Create a temporary file path
  tmp_path <- tempfile(fileext = ".xlsx")

  # 3. Test Template Generation
  # We expect this to run without error and create a file
  expect_error(
    create_ncai_template(
      template_out = tmp_path,
      habitats_label_tree = toy_hab_tree,
      es_label_tree = toy_es_tree,
      ci_names = toy_cis,
      year_list = toy_years
    ),
    NA
  )
  expect_true(file.exists(tmp_path))

  # 4. Test Template Reading
  # Read it back in and check if the structures match
  imported <- read_ncai_template(
    path = tmp_path,
    habitats_label_tree = toy_hab_tree,
    es_label_tree = toy_es_tree,
    ci_names = toy_cis
  )

  # 5. Assertions
  # Check if the cleaned habitat labels match our expectation
  expect_equal(names(imported$clean_habitats_label_tree), c("woodland", "coastal"))
  expect_equal(imported$year_list, as.character(toy_years))

  # Check that a specific matrix has the right dimensions
  expect_equal(nrow(imported$esppu_scores), 3) # oak, pine, saltmarsh
  expect_equal(ncol(imported$esppu_scores), 2) # crops, recreation

  # Cleanup
  if (file.exists(tmp_path)) unlink(tmp_path)
})
