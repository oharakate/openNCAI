
new_objects_list <- read_ncai_template("dev/NCAI_Data_Entry_Template.xlsx",
                               ns_display_habitats_label_tree,
                               ns_display_es_label_tree,
                               ns_display_ci_names)

names(new_objects_list)

# Create NS custom divisor matrix (adjustments to provision per unit)
# NOTE TO NATURESCOT: You could take this opportunity to just adjust the
# provision per unit weights in that weight matrix. I.e. divide everything
# in the affected cells by 0.2 (check that!), and not have to use this divisor
# matrix any more.
new_divisor_matrix <- ns_custom_divisor_matrix
rownames(new_divisor_matrix) <- unlist(new_objects_list$clean_habitats_label_tree, use.names = FALSE)
colnames(new_divisor_matrix) <- unlist(new_objects_list$clean_es_label_tree, use.names = FALSE)


ncai_objects <- get_ncai(habitat_extent = new_objects_list$habitat_extent,
         ci_scores = new_objects_list$ci_scores,
         habitats_label_tree = new_objects_list$clean_habitats_label_tree,
         es_label_tree = new_objects_list$clean_es_label_tree,
         year_list = new_objects_list$year_list,
         year_one = new_objects_list$year_list[1],
         provision_per_unit_scores = new_objects_list$provision_per_unit_scores,
         custom_divisor_matrix = new_divisor_matrix,
         between_importance_scores = new_objects_list$between_importance,
         within_importance_scores = new_objects_list$within_importance,
         ci_relevance_matrices = new_objects_list$ci_relevance_matrices,
         indicator_directory = new_objects_list$indicator_directory,
         return = "everything")

names(new_ncai_objects)
new_es_potential_base <- new_ncai_objects$es_potential_base
new_wellbeing_potential_base <- new_ncai_objects$wellbeing_potential_base
new_yearly_flow_of_es_matrices <- new_ncai_objects$yearly_flow_of_es_matrices
new_yearly_ncai_matrices <- new_ncai_objects$yearly_ncai_matrices
new_overall_ncai <- new_ncai_objects$overall_ncai
new_by_ecosystem_service_type <- new_ncai_objects$by_ecosystem_service_type
new_by_broad_habitat <- new_ncai_objects$by_broad_habitat


# Test the small fake set:
# These were already assigned:
# new_hab_tree <- list(
#   'B. Coastal Habitats' = c(
#     "Coastal vegetated shingle",
#     "Coastal dunes and sandy shores"
#   ),
#   'E. Grasslands' = c(
#     "Dry Grasslands",
#     "Mesic Grasslands"
#   )
# )
#
# new_es_tree <- list(
#   'PROVISIONING' = c(
#     "1.1 Cultivated Crops",
#     "1.2 Reared Animals And Their Outputs"
#   ),
#   'CULTURAL' = c(
#     "3.5. Existence & bequest"
#   )
# )
#
# new_display_ci_names <- c("National Water Quality Index", "AgriSCOR")


smalltest_things <- read_ncai_template("dev/test_small_complete_data.xlsx",
                                        new_hab_tree,
                                        new_es_tree,
                                        new_display_ci_names)
# Check:
names(smalltest_things)
smalltest_things$clean_es_label_tree
smalltest_things$clean_habitats_label_tree
smalltest_things$year_list
smalltest_things$between_importance
smalltest_things$within_importance
View(smalltest_things$indicator_directory)
View(smalltest_things$habitat_extent)
View(smalltest_things$ci_scores)
View(smalltest_things$provision_per_unit_scores)
View(smalltest_things$ci_relevance_matrices[[1]])
View(smalltest_things$ci_relevance_matrices[[2]])
