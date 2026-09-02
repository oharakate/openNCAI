ls("package:openNCAI")[grepl("^ns_", ls("package:openNCAI"))]



ncai_objects <- get_ncai(
  habitat_extent = ns_habitat_extent,
  ci_scores = ns_ci_scores,
  habitats_label_tree = ns_habitats_label_tree,
  es_label_tree = ns_es_label_tree,
  year_list = ns_year_list,
  provision_per_unit_scores = ns_provision_per_unit_scores,
  custom_divisor_matrix = ns_custom_divisor_matrix,
  between_importance_scores = ns_between_importance_scores,
  within_importance_scores = ns_within_importance_scores,
  ci_relevance_matrices = ns_ci_relevance_matrices,
  indicator_directory = ns_indicator_directory,
  return = "everything"
)

# How long does that take?

system.time(
  ncai_objects <- get_ncai(
    habitat_extent = ns_habitat_extent,
    ci_scores = ns_ci_scores,
    habitats_label_tree = ns_habitats_label_tree,
    es_label_tree = ns_es_label_tree,
    year_list = ns_year_list,
    provision_per_unit_scores = ns_provision_per_unit_scores,
    custom_divisor_matrix = ns_custom_divisor_matrix,
    between_importance_scores = ns_between_importance_scores,
    within_importance_scores = ns_within_importance_scores,
    ci_relevance_matrices = ns_ci_relevance_matrices,
    indicator_directory = ns_indicator_directory,
    return = "everything"
  )
)


