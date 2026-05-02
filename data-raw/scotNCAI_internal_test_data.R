## Importing post-calculation data from NatureScot's spreadsheet for internal
## testing use
## Kate O'Hara
## 11-Feb-2026

# In the event that we may not proceed with making this form of
# the data public, note that the final indices are published at:
# https://www.nature.scot/sites/default/files/2025-02/official-statistics-ncai-2025-summarised-data.csv

library(devtools)

# Get various post-calculation items from the NatureScot spreadsheet:
ns_sheets_path <- file.path("data-raw", "ncai_corrected.xlsx")

raw_imports <- openNCAI:::import_ns_testing_data(
  path = ns_sheets_path,
  habitats_label_tree = ns_habitats_label_tree,
  es_label_tree = ns_es_label_tree,
  year_list = 2000:2022)
names(raw_imports)
# list2env(scotNCAItestobjects, envir = .GlobalEnv)

# Use manual method to force overwrite
ref_es_potential_base <- raw_imports$ref_es_potential_base
ref_wellbeing_potential_base <- raw_imports$ref_wellbeing_potential_base
ref_total_indicator_relevances <- raw_imports$ref_total_indicator_relevances
ref_all_year_sheets <- raw_imports$ref_all_year_sheets
ref_index_breakdowns <- raw_imports$ref_index_breakdowns

# Import to data-raw:
usethis::use_data(
  ref_es_potential_base,
  ref_wellbeing_potential_base,
  ref_total_indicator_relevances,
  ref_all_year_sheets,
  ref_index_breakdowns,
  overwrite = TRUE,
  internal = TRUE
)
