## Recreating Scotland's NCAI in R
## Kate O'Hara, Chris Littleboy
## 03-12-2025


#### SETUP ####

library(openNCAI)
library(ggplot2)
library(ggthemes)

#### IMPORT NATURESCOT INPUT DATA ####
# These are the input data, i.e.,
# - Environmental measurements (habitat extent and condition indicators)
# - Weighting information (ecosystem service provision potential scores,
# importance scores, condition indicator relevance scores)
# - Metadata (habitats label tree, ecosystem services label tree, condition
# indicator labels, year list, TIR constant [2 is assumed])
# Location of the spreadsheet:
ns_sheets_path <- file.path("inst", "extdata", "ncai_corrected.xlsx")
# Get the data:
ns_data_objects <- openNCAI::import_ns_data(path = ns_sheets_path)
# See what's in the list of objects returned:
names(ns_data_objects)
# Place standalone objects into the environment for ease of access:
list2env(ns_data_objects, envir = .GlobalEnv)


#### IMPORT NATURESCOT REFERENCE CALCULATIONS FOR TESTING ####
# These are data which are the results of calculation made using NatureScot
# spreadsheet method. These are reference examples used to check that openNCAI
# replicates the NatureScot method correctly.
# Get the data:
ns_test_data_objects <- openNCAI::import_ns_testing_data(
  path = ns_sheets_path,
  habitats_label_tree = ns_habitats_label_tree,
  es_label_tree = ns_es_label_tree,
  year_list = 2000:2022)
# See what's in the list of objects created:
names(ns_test_data_objects)
# Place into environment
list2env(ns_test_data_objects, envir = .GlobalEnv)



#### CALCULATE BASES ####

## RECREATE THE ECOSYSTEM SERVICE POTENTIAL BASE
# We use openNCAI::calc_potential_weights() along with our NatureScot
# custom divisor matrix to convert the exemplary service potential scores
# (sheet 3 "ES Potential per SPU) to weights, by dividing by the number
# specified in our custom divisor matrix (normally 5, except for the cells
# marked in red in sheet6 "ES Potential Base").
made_esppu_weights <- openNCAI::calc_potential_weights(
  esppu = ns_esppu,
  custom_divisor_matrix = ns_custom_divisor_matrix,
  habitats_label_tree = ns_habitats_label_tree,
  es_label_tree = ns_es_label_tree
  )


## We use these weights and the Scottish habitat extent data with
# openNCAI::calc_espb() to recreate the Ecosystem Service Potential Base (ESPB):
made_espb <- openNCAI::calc_espb(habitat_extent = ns_habitat_extent,
                                 esppu_weights = made_esppu_weights,
                                 year_list = ns_year_list,
                                 habitats_label_tree = ns_habitats_label_tree,
                                 es_label_tree = ns_es_label_tree)
# The ESPB can be  understood as the ecosystem services provided by Scotland's
# habitats in year  one of the index.

# Does our made_espb match the published ref_espb?
all.equal(made_espb, ref_espb)
# Yes



## RECREATING THE WELL-BEING BASE
# The well-being base uses the importance scores found in sheet 4 "ES
# Potential (Weighting), which denote the importance of ecosystem services.
# The importance weights are informed by expert opinion (and a public survey?)
# and are expressed both within and between ecosystem service type groups.
# E.g. Between ecosystem service types, the Regulation & Maintenance group is
# assigned a score of 20/20 as the most important, and relative to this both
# the Provisioning and Cultural services are scored 10/20. Dividing these
# scores by the sum of 40 results in weights of 50%, 25% and 25% respectively.

# Within each service type group, a similar approach is applied, with the most
# important service given a score of 20/20 and other services given a relative
# score out of 20. The scores are converted to weights which combine the
# within- and between-service-type group importances. The result is a vector
# weights, one per ecosystem service, which sums to 100.

# openNCAI takes the between-service-type scores and the within-service-type
# scores. The between weights should be a named list of weight per service type.
# The within weights is passed as a named list of named lists, where the top
# level names are the service types, and each object holds a named list of
# individual service scores. The es_label_tree is used to check that all
# expected scores are present.
made_importance_weights <- openNCAI::calc_importance_weights(
  between_scores = ns_between_importance_scores,
  within_scores = ns_within_importance_scores,
  es_label_tree = ns_es_label_tree
)

# These should sum to 100:
sum(made_importance_weights)
# They do.


# To recreate the Well-being Base (sheet 7), we use
# openNCAI::calc_wellbeing_base(), which multiplies the importance weights by
# the Ecosystem Service Potential Base. We pass in both label trees so that
# the returned data frame is labelled.
made_wellbeing_base <- openNCAI::calc_wellbeing_base(espb = made_espb,
                              importance_weights = made_importance_weights,
                              habitats_label_tree = ns_habitats_label_tree,
                              es_label_tree = ns_es_label_tree)

# Is the calculated wellbeing base equal to NatureScot's wellbeing base?
all.equal(made_wellbeing_base, ref_wellbeing_base)
# Yes.



## CALCULATE FLOW RATE
# Flow rate of ecosystem services from habitats is estimated using
# Condition Indicator scores. These are marked relevant or irrelevant to each
# habitat/service combination and weighted by their relevance in
# the different ecosystem service type groups.

# We use the function calc_flow_rate().
made_tyfs_list <- calc_flow_rate(
  cirm_list = ns_cirms_list,
  indicator_directory = ns_indicator_directory,
  es_label_tree = ns_es_label_tree,
  habitats_label_tree = ns_habitats_label_tree,
  ci_score_matrix = ns_ci_score_matrix,
  year_list = ns_year_list,
  tir_constant = ns_tir_constant)

# We have a named list of data frames recording yearly flow rate per habitat/
# service combination:
# names(made_tyfs_list)
# head(made_tyfs_list[[4]])


## CALCULATE TOTAL ASSETS
# Now that we have the habitat extent data, and the yearly flow rate of services
# from the habitats based on condition, we can calculate the yearly assets:

made_ncai_year_matrices <- build_all_ncai_matrices(
  tyf_list = made_tyfs_list,
  wellbeing_base = made_wellbeing_base,
  habitat_extent = ns_habitat_extent,
  year_one = ns_year_list[[1]],
  habitat_labels = ns_all_habitat_labels
)
# We have one data frame for each year in shape rows=habitats / columns=
# ecosystem services.
# names(made_ncai_year_matrices)
# made_ncai_year_matrices[[10]]

# Check if our calculations match those of NatureScot across the years:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)
}, made_ncai_year_matrices[1:23], ref_all_year_sheets[1:23], SIMPLIFY = FALSE)

# They do:
comparison_results



## CALCULATE INDEXED NATURAL CAPITAL ASSETS
# From the yearly total assets matrices, we can calculate the main index, and
# the breakdowns by ecosystem service type and by broad habitat.

made_overall_index <- calc_ncai(total_assets_matrix_list =
                                  made_ncai_year_matrices)

# This can be compared to the first entry (named "overall") in the imported
# ns_index_breakdowns.
# NB displayed raw total in NS sheet is divided by 100 so, for comparison:
test_ref_overall_index <- ref_index_breakdowns[["overall"]] |>
  dplyr::mutate(raw_total = raw_total * 100) |>
  # Also, and we don't know if this is a mistake or not, no raw index is shown
  # for the overall index. Instead the smoothed value is displayed in that
  # column, so let's deselect that column in both for testing:
  dplyr::select(-raw_index)
# Use same rownames:
rownames(test_ref_overall_index) <- ns_year_list

test_made_overall_index <- made_overall_index |>
  dplyr::select(-raw_index)
all.equal(test_made_overall_index, test_ref_overall_index)
remove(test_made_overall_index)
remove(test_ref_overall_index)


# The index broken down by service type:
made_index_by_st <- calc_ncai_by_st(
  total_assets_matrix_list = made_ncai_year_matrices,
  es_label_tree_list = ns_es_label_tree)

# We can compare with the index breakdowns in the NatureScot sheet:
ref_index_by_st <- ref_index_breakdowns[names(ns_es_label_tree)] |>
  lapply(function(df) {
    rownames(df) <- ns_year_list
    return(df)
  })

# Are they equal?
all.equal(made_index_by_st, ref_index_by_st)
# Yes.


# For Scotland, the index broken down by broad habitat is:
made_index_by_bh <- calc_ncai_by_bh(
  total_assets_matrix_list = made_ncai_year_matrices,
  habitats_label_tree = ns_habitats_label_tree)

# We can compare with the index breakdowns in the NatureScot sheet.
# In NatureScot's spreadsheet, breakdowns of the NCAI are calculated for a
# selection of the broad habitats (H unvegetated and K montane groups are
# not calculated/reported), so we make a list of the broad habitats in use in
# the NatureScot sheet
# We create a list of all breakdowns in use:
ns_bh_breakdown_list <- c(ns_broad_habitats[c(1:6, 8)])
# And get that subset from the imports, make sure rownames as character:
ref_index_by_bh <- ref_index_breakdowns[ns_bh_breakdown_list] |>
  lapply(function(df) {
    rownames(df) <- ns_year_list
    return(df)
  })
# We take the similar subset from those we just calculated:
made_bh_breakdowns <- made_index_by_bh[ns_bh_breakdown_list]

# Are they equal?
all.equal(made_bh_breakdowns, ref_index_by_bh)
# Yes.







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
  "b_inland_surface_waters"
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
  "montane"
  = "Montane"
)


# Just the overall index:
main_index_for_plot <- made_overall_index |>
  tibble::rownames_to_column(var = "year") |>
  dplyr::mutate(
    year = as.numeric(year),
    breakdown = "overall",
    display_name = dplyr::recode(breakdown, !!!graph_labels)
  )

# Breakdown by ecosystem service type + main trend
by_st_for_plot <- made_index_by_st[ns_service_types] |>
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
by_bh_for_plot <- made_index_by_bh[ns_bh_breakdown_list] |>
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
  theme_classi()
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


#### HABITAT SHEETS AT THE END ####
# In these sheets we find, for each broad habitat:
# A table with a row for each year and a column containing indexed condition
# scores for each indicator relevant to that broad habitat.

# Across the top of that table is a vector of values recording the 'influence'
# of each condition indicator.
# At the bottom is calculated the 'influence since 2000' for each indicator.
# This = (latest value - year one value) * influence.
# And a similar statistic for 'since 2019'.

# Where does the 'influence' figure come from?
# It's the sum of the row totals for all rows in that broad habitat from the
# second table of the CI sheets.
# That table is found to the right of the
# main one in the CI sheets and the cell values are calculated as:
# (ciwm cell / tir cell)
# * wellbeing base cell
# / sum of wellbeing base rows in that habitat.

# Look at indicator 67 since this is relevant to both grassland (multi-row)
# and cropland (single row).

# There is a further table to the right of that where cell values are
# calculated as:
# (ciwm cell / tir cell)
# * wellbeing base cell
# / sum of that service column in wellbeing base.

# We then also have a breakdown by level-2 habitat type, where there is more
# than one in the broad habitat.

# For now, we will leave the relevances. They are certainly calculable, but
# let's work on transposing the indexed CI scores and plotting.

# We are going to need to know which indicators are relevant to each broad
# habitat.
# FUNCTION indicators_to_get() reads the whole list of CIWMs and returns a list
# of indicator IDs relevant to a broad habitat:
indicators_to_get <- function(broad_habitat,
                              habitats_label_tree,
                              all_ciwms_list,
                              all_habitat_labels) {

  # Identify habitat rows within the broad habitat type:
  bh_row_group <- habitats_label_tree[[broad_habitat]]

  indicators_to_get <- sapply(all_ciwms_list, function(ciwm) {
    # Ensure rownames are set:
    rownames(ciwm) <- all_habitat_labels
    # Calculate sum of relevant row sums
    s <- sum(ciwm[bh_row_group, ], na.rm = TRUE)
    # Return true if not NA & > 0.
    return(!is.na(s) && s > 0)
  })

  # Return names of relevant indicators:
  return(names(all_ciwms_list[indicators_to_get]))
}

# E.g. this should return the list of relvant CI IDs for cropland.
# test_indicators_to_get <- indicators_to_get(
#   broad_habitat = "cropland",
#   habitats_label_tree = habitats_label_tree,
#   all_ciwms_list = ns_all_ciwms_list,
#   all_habitat_labels)

build_bh_condition_tables <- function(habitats_label_tree,
                                      all_ciwms_list,
                                      ci_score_matrix,
                                      all_habitat_labels,
                                      year_list,
                                      year_one = year_list[[1]],
                                      habitats_to_process =
                                        names(habitats_label_tree)) {

  # Check if custom year one is in the list
  baseline_row_index <- match(year_one, year_list)

  if (is.na(baseline_row_index)) {
    stop("The provided year_one is not found in the year_list.")
  }

  # For each habitat, subset the CI scores matrix to just the relevant
  # indicators:
  bh_condition_tables <- lapply(habitats_to_process, function(bh) {

    relevant_indicators <- indicators_to_get(
      broad_habitat = bh,
      habitats_label_tree = habitats_label_tree,
      all_ciwms_list = all_ciwms_list,
      all_habitat_labels = all_habitat_labels
    )

    valid_columns <- intersect(relevant_indicators, colnames(ci_score_matrix))
    subset_matrix <- ci_score_matrix[, valid_columns, drop = FALSE]

    # Name rows by year
    rownames(subset_matrix) <- year_list

    # Index scores on year one
    indexed_matrix <- as.data.frame(lapply(subset_matrix, function(col) {
      # Divide every value in the column by the value at the baseline year
      col / col[baseline_row_index] * 100
    }))

    # Make sure names are restored
    rownames(indexed_matrix) <- year_list
    colnames(indexed_matrix) <- valid_columns

    return(indexed_matrix)

  })
  names(bh_condition_tables) <- habitats_to_process

  return(bh_condition_tables)
}

# Get the tables for all broad habitats, as per NatureScot spreadsheet:
scot_bh_condition_tables <- build_bh_condition_tables(
  habitats_label_tree = habitats_label_tree,
  all_ciwms_list = ns_all_ciwms_list,
  ci_score_matrix = ns_ci_score_matrix,
  all_habitat_labels = all_habitat_labels,
  year_list = ns_year_list
)
scot_bh_condition_tables[[1]]

# And we would be able to plot these as per the plot top right in the Coastal
# sheet...
# Reshape data:
coastal_plot_data <- scot_bh_condition_tables[["coastal"]] %>%
  tibble::rownames_to_column(var = "year") %>%
  mutate(year = as.numeric(year)) %>%
  pivot_longer(
    cols = -year,
    names_to = "indicator_id",
    values_to = "index_value"
  )

# Here is a basic and not-tidied up version of the Coastal indicators plot:
ggplot(coastal_plot_data, aes(x = year, y = index_value, color = indicator_id)) +
  geom_line(linewidth = 1) +
  geom_point() +
  # Add a horizontal line at 100 to show the baseline clearly
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  labs(
    title = "Indicator Trends: Coastal Habitat",
    subtitle = "Indexed to Year One (100)",
    x = "Year",
    y = "Index Value",
    color = "Indicator ID"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save it:
ggsave(
  filename = file.path("dev", "coastal_indicator_trends.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 4,               # Height in inches
  dpi = 300                 # High resolution for reports
)


