## Importing input data from NatureScot's spreadsheet for use in examples
## Kate O'Hara
## 11-Feb-2026



# Import various input data, metadata and weights, ensuring it is labelled and
# in the correct format:
ns_sheets_path <- file.path("inst", "extdata", "ncai_corrected.xlsx")
ns_data_objects <- openNCAI:::import_ns_data(path = ns_sheets_path)
names(ns_data_objects)
list2env(ns_data_objects, envir = .GlobalEnv)

# Add the objects tot he package data dir:
usethis::use_data(ns_habitat_extent,
                  ns_ci_score_matrix,
                  ns_habitats_label_tree,
                  ns_es_label_tree,
                  ns_year_list,
                  ns_esppu,
                  ns_custom_divisor_matrix,
                  ns_between_importance_scores,
                  ns_within_importance_scores,
                  ns_cirms_list,
                  ns_indicator_directory,
                  overwrite = TRUE)
