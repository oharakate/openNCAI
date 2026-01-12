# Recreating Scotland's NCAI in R, now working with the corrected version of the
# NatureScot spreadsheet
# Kate O'Hara
# 12-01-2026

# We will need a path to the corrected sheet:
ns_corrected_sheets_path <- file.path("dev", "ncai_corrected.xlsx")

# We need the corrected version of the Condition Indicator scores matrix:
ns_corrected_ci_score_matrix <- read_the_ci_scores(sheet_path = ns_corrected_sheets_path,
                                         sheet_list = 9:46,
                                         vector_range = "I36:I58")

# We need the corrected version of the year sheets:
ns_corrected_all_year_sheets <- lapply(X = ns_year_sheets_ids,
                             FUN = read_ns_year_sheet,
                             path = ns_corrected_sheets_path,
                             labels = all_service_labels)

# Labels where needed:
rownames(ns_corrected_ci_score_matrix) <- ns_year_list

# Calculate flow again:
scot_corrected_tyfs_list <- build_all_tyfs(raw_cis = ns_corrected_ci_score_matrix,
                                 year_list = ns_year_list,
                                 ciwms_list = ns_all_ciwms_list,
                                 tir = scot_tir,
                                 tir_constant = ns_tir_constant)

# Calculate assets again:
scot_corrected_ncai_list <- build_all_ncai_matrices(
  tyf_list = scot_corrected_tyfs_list,
  wellbeing_base = ns_wellbeing_base,
  habitat_extent = ns_habitat_extent,
  year_one = ns_year_list[[1]]
)

# Compare results again:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)},
  scot_corrected_ncai_list[1:23],
  ns_corrected_all_year_sheets[1:23],
  SIMPLIFY = FALSE)
comparison_results
