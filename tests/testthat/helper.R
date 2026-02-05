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
