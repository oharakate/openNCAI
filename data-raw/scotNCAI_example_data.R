## Importing input data from NatureScot's spreadsheet for use in examples
## Kate O'Hara
## 11-Feb-2026



# Import various input data, metadata and weights, ensuring it is labelled and
# in the correct format:
ns_sheets_path <- file.path("data-raw", "ncai_corrected.xlsx")
raw_import <- openNCAI:::import_ns_data(path = ns_sheets_path)
names(ns_data_objects)
# list2env(ns_data_objects, envir = .GlobalEnv)

# Force refresh
ns_habitat_extent <- raw_import$ns_habitat_extent
ns_ci_scores <- raw_import$ns_ci_scores
ns_habitats_label_tree <- raw_import$ns_habitats_label_tree
ns_es_label_tree <- raw_import$ns_es_label_tree
ns_year_list <- raw_import$ns_year_list
ns_esppu_scores <- raw_import$ns_esppu_scores
ns_custom_divisor_matrix <- raw_import$ns_custom_divisor_matrix
ns_between_importance_scores <- raw_import$ns_between_importance_scores
ns_within_importance_scores <- raw_import$ns_within_importance_scores
ns_ci_relevance_matrices <- raw_import$ns_ci_relevance_matrices
ns_indicator_directory <- raw_import$ns_indicator_directory


# Add the objects tot he package data dir:
usethis::use_data(ns_habitat_extent,
                  ns_ci_scores,
                  ns_habitats_label_tree,
                  ns_es_label_tree,
                  ns_year_list,
                  ns_esppu_scores,
                  ns_custom_divisor_matrix,
                  ns_between_importance_scores,
                  ns_within_importance_scores,
                  ns_ci_relevance_matrices,
                  ns_indicator_directory,
                  overwrite = TRUE)




