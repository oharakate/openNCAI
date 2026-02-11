## Importing post-calculation data from NatureScot's spreadsheet for internal
## testing use
## Kate O'Hara
## 11-Feb-2026

# In the event that we may not proceed with making this form of
# the data public, note that the final indices are published at:
# https://www.nature.scot/sites/default/files/2025-02/official-statistics-ncai-2025-summarised-data.csv

library(devtools)

# Get various post-calculation items from the NatureScot spreadsheet:
ns_sheets_path <- file.path("inst", "extdata", "ncai_corrected.xlsx")

scotNCAItestobjects <- openNCAI:::import_ns_testing_data(
  path = ns_sheets_path,
  habitats_label_tree = ns_habitats_label_tree,
  es_label_tree = ns_es_label_tree,
  year_list = 2000:2022)
names(scotNCAItestobjects)
list2env(scotNCAItestobjects, envir = .GlobalEnv)

# Create a matrix of zeroes in the same shape as the test data matrices:
zero_main_matrix <- ref_espb
zero_main_matrix[] <- 0

# Import to data-raw:
usethis::use_data(
  ref_espb,
  ref_wellbeing_base,
  ref_tir,
  ref_all_year_sheets,
  ref_index_breakdowns,
  zero_main_matrix,
  overwrite = TRUE,
  internal = TRUE
)
