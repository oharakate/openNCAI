## Recreating Scotland's NCAI in R
## Kate O'Hara, Chris Littleboy
## 03-12-2025


#### SETUP ####

library(openNCAI)
library(ggplot2)
library(ggthemes)


#### DATA IS BUNDLED WITH THE PACKAGE ####
# The NatureScot input data is lazy loaded so just call objects.
# See the list of functions and data objects like this:
ls("package:openNCAI")

# Infos on functions and data:
help(package = "openNCAI")

# Get the test objects (post-calculation objects from NS spreadsheet)
load("R/sysdata.rda")


#### Calculate everything with the big function ####
ncai_objects <- get_ncai(habitat_extent = ns_habitat_extent,
                         ci_scores = ns_ci_scores,
                         habitats_label_tree = ns_habitats_label_tree,
                         es_label_tree = ns_es_label_tree,
                         year_list = ns_year_list,
                         esppu_scores = ns_esppu_scores,
                         custom_divisor_matrix = ns_custom_divisor_matrix,
                         between_importance_scores = ns_between_importance_scores,
                         within_importance_scores = ns_within_importance_scores,
                         ci_relevance_matrices = ns_ci_relevance_matrices,
                         indicator_directory = ns_indicator_directory,
                         return = "everything")


# Get without custom esppu division for checking something
# ncai_objects <- get_ncai(habitat_extent = ns_habitat_extent,
#                          ci_scores = ns_ci_scores,
#                          habitats_label_tree = ns_habitats_label_tree,
#                          es_label_tree = ns_es_label_tree,
#                          year_list = ns_year_list,
#                          esppu_scores = ns_esppu_scores,
#                          esppu_divisor = 5,
#                          between_importance_scores = ns_between_importance_scores,
#                          within_importance_scores = ns_within_importance_scores,
#                          ci_relevance_matrices = ns_ci_relevance_matrices,
#                          indicator_directory = ns_indicator_directory,
#                          return = "everything")


# What did we get?
names(ncai_objects)

# Check making ESPB works:
# Does our made_espb match the published ref_espb?
all.equal(ncai_objects$espb, ref_espb)

# Check making Wellbeing Base works:
# Is the calculated wellbeing base equal to NatureScot's wellbeing base?
all.equal(ncai_objects$wellbeing_base, ref_wellbeing_base)
# Yes.

# Total Yearly Flow is not displayed in NatureScot method, but we use it to
# get the total yearly asset matrices and we can check those:
# Check if our calculations match those of NatureScot across the years:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)
}, ncai_objects$yearly_asset_matrices[1:23],
ref_all_year_sheets[1:23],
SIMPLIFY = FALSE)
# They do:
comparison_results

# The overall index is found at:
ncai_objects$overall_index
# This can be compared to the first entry (named "overall") in the imported
# ns_index_breakdowns.
# BUT NOTE THAT displayed raw total in NS sheet is divided by 100 so,
# for comparison:
test_ref_overall_index <- ref_index_breakdowns[["overall"]] |>
  dplyr::mutate(raw_total = raw_total * 100) |>
  # Also, and we don't know if this is a mistake or not, no raw index is shown
  # for the overall index. Instead the smoothed value is displayed in that
  # column, so let's deselect that column in both for testing:
  dplyr::select(-raw_index)

test_our_overall_index <- ncai_objects$overall_index |>
  dplyr::select(-raw_index)
all.equal(test_our_overall_index, test_ref_overall_index)
remove(test_our_overall_index)
remove(test_ref_overall_index)

# Check if our index broken down by ecosystem service type matches the ref:
# Are they equal?
ref_index_by_st <- ref_index_breakdowns[names(ns_es_label_tree)]
all.equal(ncai_objects$by_ecosystem_service_type, ref_index_by_st)
# Yes.

# Same check for index broken down by broad habitat:
# Not all broad habitat breakdowns are there in the reference set:
names(ref_index_breakdowns)
# Make a list of the ones we can compare:
ns_bh_breakdowns <- names(ref_index_breakdowns[5:11])
ref_index_by_bh <- ref_index_breakdowns[ns_bh_breakdowns]
all.equal(ncai_objects$by_broad_habitat[ns_bh_breakdowns], ref_index_by_bh)
# All matching.

# The function works!


#### CALCULATE BASES collapse and ignore this bit ####
#
# ## RECREATE THE ECOSYSTEM SERVICE POTENTIAL BASE
# # We use openNCAI::calc_potential_weights() along with our NatureScot
# # custom divisor matrix to convert the exemplary service potential scores
# # (sheet 3 "ES Potential per SPU) to weights, by dividing by the number
# # specified in our custom divisor matrix (normally 5, except for the cells
# # marked in red in sheet6 "ES Potential Base").
# made_esppu_weights <- openNCAI::calc_potential_weights(
#   esppu = ns_esppu_scores,
#   custom_divisor_matrix = ns_custom_divisor_matrix,
#   habitats_label_tree = ns_habitats_label_tree,
#   es_label_tree = ns_es_label_tree
#   )
#
#
# ## We use these weights and the Scottish habitat extent data with
# # openNCAI::calc_espb() to recreate the Ecosystem Service Potential Base (ESPB):
# made_espb <- openNCAI::calc_espb(habitat_extent = ns_habitat_extent,
#                                  esppu_weights = made_esppu_weights,
#                                  year_list = ns_year_list,
#                                  habitats_label_tree = ns_habitats_label_tree,
#                                  es_label_tree = ns_es_label_tree)
# # The ESPB can be  understood as the ecosystem services provided by Scotland's
# # habitats in year  one of the index.
#
# # Does our made_espb match the published ref_espb?
# all.equal(made_espb, ref_espb)
# # Yes
#
#
#
# ## RECREATING THE WELL-BEING BASE
# # The well-being base uses the importance scores found in sheet 4 "ES
# # Potential (Weighting), which denote the importance of ecosystem services.
# # The importance weights are informed by expert opinion (and a public survey?)
# # and are expressed both within and between ecosystem service type groups.
# # E.g. Between ecosystem service types, the Regulation & Maintenance group is
# # assigned a score of 20/20 as the most important, and relative to this both
# # the Provisioning and Cultural services are scored 10/20. Dividing these
# # scores by the sum of 40 results in weights of 50%, 25% and 25% respectively.
#
# # Within each service type group, a similar approach is applied, with the most
# # important service given a score of 20/20 and other services given a relative
# # score out of 20. The scores are converted to weights which combine the
# # within- and between-service-type group importances. The result is a vector
# # weights, one per ecosystem service, which sums to 100.
#
# # openNCAI takes the between-service-type scores and the within-service-type
# # scores. The between weights should be a named list of weight per service type.
# # The within weights is passed as a named list of named lists, where the top
# # level names are the service types, and each object holds a named list of
# # individual service scores. The es_label_tree is used to check that all
# # expected scores are present.
# made_importance_weights <- openNCAI::calc_importance_weights(
#   between_scores = ns_between_importance_scores,
#   within_scores = ns_within_importance_scores,
#   es_label_tree = ns_es_label_tree
# )
#
# # These should sum to 100:
# sum(made_importance_weights)
# # They do.
#
#
# # To recreate the Well-being Base (sheet 7), we use
# # openNCAI::calc_wellbeing_base(), which multiplies the importance weights by
# # the Ecosystem Service Potential Base. We pass in both label trees so that
# # the returned data frame is labelled.
# made_wellbeing_base <- openNCAI::calc_wellbeing_base(espb = made_espb,
#                               importance_weights = made_importance_weights,
#                               habitats_label_tree = ns_habitats_label_tree,
#                               es_label_tree = ns_es_label_tree)
#
# # Is the calculated wellbeing base equal to NatureScot's wellbeing base?
# all.equal(made_wellbeing_base, ref_wellbeing_base)
# # Yes.
#
#
#
# ## CALCULATE FLOW RATE
# # Flow rate of ecosystem services from habitats is estimated using
# # Condition Indicator scores. These are marked relevant or irrelevant to each
# # habitat/service combination and weighted by their relevance in
# # the different ecosystem service type groups.
#
# # We use the function calc_flow_rate().
# made_tyfs_list <- calc_flow_rate(
#   cirm_list = ns_ci_relevance_matrices,
#   indicator_directory = ns_indicator_directory,
#   es_label_tree = ns_es_label_tree,
#   habitats_label_tree = ns_habitats_label_tree,
#   ci_scores = ns_ci_scores,
#   year_list = ns_year_list,
#   tir_constant = ns_tir_constant)
#
# # We have a named list of data frames recording yearly flow rate per habitat/
# # service combination:
# # names(made_tyfs_list)
# # head(made_tyfs_list[[4]])
#
#
# ## CALCULATE TOTAL ASSETS
# # Now that we have the habitat extent data, and the yearly flow rate of services
# # from the habitats based on condition, we can calculate the yearly assets:
#
# made_ncai_year_matrices <- build_all_ncai_matrices(
#   tyf_list = made_tyfs_list,
#   wellbeing_base = made_wellbeing_base,
#   habitat_extent = ns_habitat_extent,
#   year_one = ns_year_list[[1]],
#   habitat_labels = ns_all_habitat_labels
# )
# # We have one data frame for each year in shape rows=habitats / columns=
# # ecosystem services.
# # names(made_ncai_year_matrices)
# # made_ncai_year_matrices[[10]]
#
# # Check if our calculations match those of NatureScot across the years:
# comparison_results <- mapply(function(list1, list2) {
#   all.equal(list1, list2)
# }, made_ncai_year_matrices[1:23], ref_all_year_sheets[1:23], SIMPLIFY = FALSE)
#
# # They do:
# comparison_results
#
#
#
# ## CALCULATE INDEXED NATURAL CAPITAL ASSETS
# # From the yearly total assets matrices, we can calculate the main index, and
# # the breakdowns by ecosystem service type and by broad habitat.
#
# made_overall_index <- calc_ncai(total_assets_matrix_list =
#                                   made_ncai_year_matrices)
#
# # This can be compared to the first entry (named "overall") in the imported
# # ns_index_breakdowns.
# # NB displayed raw total in NS sheet is divided by 100 so, for comparison:
# test_ref_overall_index <- ref_index_breakdowns[["overall"]] |>
#   dplyr::mutate(raw_total = raw_total * 100) |>
#   # Also, and we don't know if this is a mistake or not, no raw index is shown
#   # for the overall index. Instead the smoothed value is displayed in that
#   # column, so let's deselect that column in both for testing:
#   dplyr::select(-raw_index)
# # Use same rownames:
# rownames(test_ref_overall_index) <- ns_year_list
#
# test_made_overall_index <- made_overall_index |>
#   dplyr::select(-raw_index)
# all.equal(test_made_overall_index, test_ref_overall_index)
# remove(test_made_overall_index)
# remove(test_ref_overall_index)
#
#
# # The index broken down by service type:
# made_index_by_st <- calc_ncai_by_st(
#   total_assets_matrix_list = made_ncai_year_matrices,
#   es_label_tree_list = ns_es_label_tree)
#
# # We can compare with the index breakdowns in the NatureScot sheet:
# ref_index_by_st <- ref_index_breakdowns[names(ns_es_label_tree)] |>
#   lapply(function(df) {
#     rownames(df) <- ns_year_list
#     return(df)
#   })
#
# # Are they equal?
# all.equal(made_index_by_st, ref_index_by_st)
# # Yes.
#
#
# # For Scotland, the index broken down by broad habitat is:
# made_index_by_bh <- calc_ncai_by_bh(
#   total_assets_matrix_list = made_ncai_year_matrices,
#   habitats_label_tree = ns_habitats_label_tree)
#
# # We can compare with the index breakdowns in the NatureScot sheet.
# # In NatureScot's spreadsheet, breakdowns of the NCAI are calculated for a
# # selection of the broad habitats (H unvegetated and K montane groups are
# # not calculated/reported), so we make a list of the broad habitats in use in
# # the NatureScot sheet
# # We create a list of all breakdowns in use:
# ns_bh_breakdown_list <- c(ns_broad_habitats[c(1:6, 8)])
# # And get that subset from the imports, make sure rownames as character:
# ref_index_by_bh <- ref_index_breakdowns[ns_bh_breakdown_list] |>
#   lapply(function(df) {
#     rownames(df) <- ns_year_list
#     return(df)
#   })
# # We take the similar subset from those we just calculated:
# made_bh_breakdowns <- made_index_by_bh[ns_bh_breakdown_list]
#
# # Are they equal?
# all.equal(made_bh_breakdowns, ref_index_by_bh)
# # Yes.
####








#### PLOT INDICES ####

# We can plot the main index.
# Datasets for plotting...

# First make display labels:
graph_labels <- c(
  "overall"
  = "Overall",
  # --- Service Types ---
  "provisioning"
  = "Provisioning",
  "regulation_and_maintenance"
  = "Regulation & Maintenance",
  "cultural"
  = "Cultural",
  # --- Broad Habitats ---
  "b_coastal_habitats"
  = "Coastal",
  "c_inland_surface_waters"
  = "Freshwater",
  "d_mires_bogs_and_fens"
  = "Mires, Bogs & Fens",
  "e_grasslands_and_lands_dominated_by_forbs_mosses_or_lichens"
  = "Grasslands",
  "f_heathland_scrub_and_tundra"
  = "Heathland",
  "g_woodland_forest_and_other_wooded_land"
  = "Woodland",
  "h_inland_unvegetated_or_sparsely_vegetated_habitats"
  = "Inland Unvegetated",
  "i_cultivated_agricultural_horticultural_and_domestic_habitats"
  = "Agri/Horticultural",
  "j_constructed_industrial_and_other_artificial_habitats"
  = "Constructed",
  "k_montane"
  = "Montane"
)

ns_bh_breakdown_list <- c(names(ns_habitats_label_tree)[c(1:6, 8)])


# Just the overall index:
main_index_for_plot <- ncai_objects$overall_index |>
  tibble::rownames_to_column(var = "year") |>
  dplyr::mutate(
    year = as.numeric(year),
    breakdown = "overall",
    display_name = dplyr::recode(breakdown, !!!graph_labels)
  )

# Breakdown by ecosystem service type + main trend
by_st_for_plot <- ncai_objects$by_ecosystem_service_type[names(ns_es_label_tree)] |>
  lapply(function(df) {
    df |>
      tibble::rownames_to_column(var = "year") |>
      dplyr::mutate(year = as.numeric(year)) # Convert here!
  }) |>
  dplyr::bind_rows(.id = "breakdown") |>
  dplyr::bind_rows(main_index_for_plot) |>
  dplyr::mutate(
    display_name = dplyr::recode(breakdown, !!!graph_labels)
  )

# Breakdown by broad habitat + main trend
# Use the subsetted data with only the broad habitats graphed by NS
by_bh_for_plot <- ncai_objects$by_broad_habitat[ns_bh_breakdown_list] |>
  lapply(function(df) {
    df |>
      tibble::rownames_to_column(var = "year") |>
      dplyr::mutate(year = as.numeric(year)) # Convert character to double here
  }) |>
  dplyr::bind_rows(.id = "breakdown") |>
  # Now both datasets have 'year' as <double>
  dplyr::bind_rows(main_index_for_plot) |>
  dplyr::mutate(
    display_name = dplyr::recode(breakdown, !!!graph_labels)
  )

# The Overall Index
# (NatureScot graph plots the unsmoothed values)
ggplot(main_index_for_plot, aes(x = year, y = raw_index)) +
  # Reverting to default ggplot2 line (black, standard thickness)
  geom_line() +

  # Standard scales
  scale_y_continuous(
    breaks = seq(90, 110, by = 2)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, by = 3)
  ) +

  # Labels
  labs(
    title = "NCAI (Overall)",
    x = "Year",
    y = "Index (Base = 100)"
  ) +

  # Classic theme
  theme_classic() +

  # Minimal centering for the title
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
# Save it:
ggsave(
  filename = file.path("dev", "ncai_overall_trend.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 6,               # Height in inches
  dpi = 300                 # High resolution for reports
)


# BY SERVICE TYPE
# The index by service type with main index for reference:
ggplot(by_st_for_plot, aes(x = year, y = raw_index, color = display_name)) +
  # Basic lines
  geom_line() +

  # Point marker for overall line to distinguish trend
  # Note: We still filter by 'breakdown' internally for reliability
  geom_point(
    data = dplyr::filter(by_st_for_plot, breakdown == "overall"),
    shape = 18,
    size = 3
  ) +

  # Labels
  labs(
    title = "NCAI by Ecosystem Service Type",
    x = "Year",
    y = "Index (Base = 100)",
    color = "Service Type"
  ) +

  # Classic theme (solid axis lines, no grid)
  theme_classic()
# Save it:
ggsave(
  filename = file.path("dev", "ncai_by_ecosystem_service_type.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 6,               # Height in inches
  dpi = 300                 # High resolution for reports
)


# The index by broad habitat with main index for reference:
ggplot(by_bh_for_plot, aes(x = year, y = raw_index, color = display_name)) +
  # Lines for all habitats
  geom_line() +

  # Diamond marker on the overall trend line
  # We filter by 'breakdown' because that remains "overall" internally
  geom_point(
    data = dplyr::filter(by_bh_for_plot, breakdown == "overall"),
    shape = 18,
    size = 3
  ) +

  # Labels
  labs(
    title = "NCAI by Broad Habitat",
    x = "Year",
    y = "Index (Base = 100)",
    color = "Habitat Type"
  ) +

  # The requested classic theme
  theme_classic()
# Save it:
ggsave(
  filename = file.path("dev", "ncai_by_broad_habitat.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 6,               # Height in inches
  dpi = 300                 # High resolution for reports
)


#### BONUS CONTENT - test the functions which pull out time series of
# esp potential and wellbeing potential
prov_time_series_mats <- get_yearly_potential_provision(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  espb = ncai_objects$espb,
  as_matrices = TRUE)
# View(prov_time_series_mats[[1]])
# View(prov_time_series_mats[["2005"]])
str(prov_time_series_mats[[1]])
str(prov_time_series_mats[["2005"]])

prov_time_series <- get_yearly_potential_provision(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  espb = ncai_objects$espb,
  as_matrices = FALSE)
head(prov_time_series)


wb_time_series <- get_yearly_potential_wellbeing(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  wellbeing_base = ncai_objects$wellbeing_base,
  as_matrices = FALSE)
head(wb_time_series)

wb_time_series_mats <- get_yearly_potential_wellbeing(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  wellbeing_base = ncai_objects$wellbeing_base,
  as_matrices = TRUE)
# View(wb_time_series_mats[[1]])
# View(wb_time_series_mats[["2005"]])
str(wb_time_series_mats[[1]])
str(wb_time_series_mats[["2005"]])

# Graph these two together:
library(ggplot2)

# 1. Combine data
plot_df <- data.frame(
  year = as.numeric(rownames(prov_time_series)),
  Provision = prov_time_series$raw_index,
  Wellbeing = wb_time_series$raw_index
)

# 2. Pivot to long format or just plot layers
ggplot(plot_df, aes(x = year)) +
  geom_line(aes(y = Provision, color = "Provision"), size = 1) +
  geom_line(aes(y = Wellbeing, color = "Wellbeing"), size = 1) +
  geom_point(aes(y = Provision, color = "Provision")) +
  geom_point(aes(y = Wellbeing, color = "Wellbeing")) +
  scale_color_manual(values = c("Provision" = "darkseagreen", "Wellbeing" = "hotpink")) +
  theme_minimal() +
  labs(title = "NCAI Potential Indices",
       x = "Year", y = "Raw Index (2000 = 100)",
       color = "Index Type")



# NB Arnab suggested the first thing he'd want to see would be the ratio
# of the two over time. So:
# 1. Add the ratio calculation
plot_df$Ratio <- plot_df$Wellbeing / plot_df$Provision

# 2. Plot the Ratio over time
ggplot(plot_df, aes(x = year, y = Ratio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue") +
  # Adding a reference line at 1.0 (where Wellbeing = Provision)
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  labs(
    title = "Ratio of Wellbeing to Provision Potential",
    subtitle = "Values < 1 indicate wellbeing potential output is not keeping up with physical potential provision",
    x = "Year",
    y = "Wellbeing / Provision"
  )


# Putting three stages of weighting together:
plot_df_fuller <- data.frame(
  year = as.numeric(rownames(prov_time_series)),
  Provision = prov_time_series$raw_index,
  Wellbeing = wb_time_series$raw_index,
  Final = ncai_objects$overall_index$raw_index
)

ggplot(plot_df_fuller, aes(x = year)) +
  # Lines
  geom_line(aes(y = Provision, color = "Potential service provision (1)"), linewidth = 1) +
  geom_line(aes(y = Wellbeing, color = "Potential wellbeing contribution (2)"), linewidth = 1) +
  geom_line(aes(y = Final, color = "Final asset value (3)"), linewidth = 1) +
  # Points
  geom_point(aes(y = Provision, color = "Potential service provision (1)")) +
  geom_point(aes(y = Wellbeing, color = "Potential wellbeing contribution (2)")) +
  geom_point(aes(y = Final, color = "Final asset value (3)")) +
#   ref line
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray70", alpha = 0.5) +
  # Scales
  scale_color_manual(values = c(
    "Potential service provision (1)" = "darkseagreen",
    "Potential wellbeing contribution (2)" = "hotpink",
    "Final asset value (3)" = "steelblue"
  )) +
  theme_minimal() +
  labs(
    title = "NCAI Potential Indices",
    x = "Year",
    y = "Raw Index (2000 = 100)",
    color = "Index Type"
  )
