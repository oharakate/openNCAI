# Demonstrates show_missing() across every input shape it dispatches on.
# Run from the package root with devtools::load_all() loaded (or source
# this whole file). Not part of the installed package (see .Rbuildignore).

devtools::load_all()

# 1. A plain data frame (habitat_extent / ci_scores / provision_per_unit_scores shape)
#    -> ggplot bar chart, rownames() used as labels.
df_example <- data.frame(
  year_2020 = c(0.9, 0.8, NA, 0.7),
  year_2021 = c(0.85, NA, 0.6, 0.7),
  year_2022 = c(0.9, 0.8, 0.65, NA)
)
rownames(df_example) <- c(
  "b1_coastal_dunes_and_sandy_shores",
  "d1_raised_and_blanket_bogs",
  "e2_mesic_grasslands",
  "g1_broadleaved_deciduous_woodland"
)
show_missing(df_example)

# 2. A data frame with a label_col override (indicator_directory shape)
#    -> ggplot bar chart, using the named column instead of rownames() as labels.
dir_example <- data.frame(
  ci_id = c("2_pollution_orthophosphate_at_safe_level", "6_woodland_bird_index", ""),
  provisioning = c(0.2, 1, 0.5),
  cultural = c(0.2, NA, 0.5)
)
show_missing(dir_example, label_col = "ci_id")

# 3. A one-level label tree, character leaves (habitats_label_tree / es_label_tree shape)
#    -> plain-text listing: blank values AND blank names both flagged.
label_tree_example <- list(
  b_coastal_habitats = c("b1_coastal_dunes_and_sandy_shores", "b2_coastal_shingle", NA),
  d_mires_bogs_and_fens = c("d1_raised_and_blanket_bogs", "  ")
)
names(label_tree_example)[2] <- ""
show_missing(label_tree_example)

# 4. A one-level weight tree, numeric leaves (between_importance_scores shape)
#    -> plain-text listing: NA leaf flagged, name still checked too.
weight_tree_example <- list(provisioning = 75, regulation_and_maintenance = NA, cultural = 25)
names(weight_tree_example)[1] <- ""
show_missing(weight_tree_example)

# 5. A two-level nested weight tree (within_importance_scores shape)
#    -> plain-text listing: nested paths shown as parent$child[i] / parent[[i]].
nested_tree_example <- list(
  provisioning = list(crops = 10, timber = NA),
  cultural = list(recreation = 5, heritage = 5)
)
names(nested_tree_example$provisioning)[1] <- ""
show_missing(nested_tree_example)

# 6. A bare vector (year_list shape)
#    -> plain-text listing of missing elements by position/name.
vector_example <- c("2020", NA, "2022", "")
show_missing(vector_example)
