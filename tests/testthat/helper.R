# Create minimal mock trees so label_ncai_matrix doesn't crash
# The NatureScot template usually has 31 habitats.
# We need to provide exactly 31 labels to satisfy the check.
ns_habitats_label_tree <- list(
  habitats = paste0("habitat_", 1:31)
)

# It usually has 28 ecosystem services.
ns_es_label_tree <- list(
  services = paste0("service_", 1:28)
)

ns_service_types <- c("provisioning", "regulation_and_maintenance", "cultural")
ns_broad_habitats <- names(ns_habitats_label_tree)
ns_year_list <- as.character(2000:2022)


# 1. Try to find the root of the package regardless of where the test is called from
pkg_root <- tryCatch(
  rprojroot::find_package_root_file(),
  error = function(e) "."
)

# 2. Construct the absolute path to data-raw
local_path <- file.path(pkg_root, "data-raw", "ncai_corrected.xlsx")

# 3. Final Check and Assignment
if (file.exists(local_path)) {
  ns_sheets_path <- local_path
} else {
  # If we are here, the file isn't at the expected path
  # We set it to empty string so skip_if logic works
  ns_sheets_path <- ""
}

