## Recreating Scotland's NCAI in R
## Kate O'Hara, Chris Littleboy
## 03-12-2025


## NOTES
# REMEMBER TO WRITE A WRAPPER FUNCTION WHICH DOES ALL NATURE SCOT IMPORT BUSINESS.
# NEXT STEP do packaging and report writing.

#### SETUP ####

# library(dplyr)
# library(tibble)
# library(tidyr)
# library(readr)
library(readxl)
# library(slider)
# library(ggplot2)
# library(janitor)
# library(openNCAI)


#### IMPORT NATURESCOT INPUT DATA ####
# These are the input data, i.e.,
# - Environmental measurements (habitat extent and condition indicators)
# - Weighting information (ecosystem service provision potential scores,
# importance scores, condition indicator relevance scores)
# - Metadata (habitats label tree, ecosystem services label tree, condition
# indicator labels, year list, TIR constant [2 is assumed])
# Location of the spreadsheet:
ns_sheets_path <- file.path("inst", "extdata", "ncai.xlsx")
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

## CALCULATING THE WEIGHTED INDICATORS MATRICES
# Relevance weights for each Condition Indicator, per ecosystem service type
# are recorded in the Indicator Directory (ns_indicator_directory). For each
# indicator, the weight is multiplied by the list Condition Indicator Relevance
# Matrices, to give a list of Condition Indicator Weight Matrices.

# Function build_ciwm_list() does this:
ns_all_ciwms_list <- openNCAI::build_ciwm_list(
  cirm_list = ns_cirms_list,
  indicator_directory = ns_indicator_directory,
  es_label_tree = ns_es_label_tree,
  habitats_label_tree = ns_habitats_label_tree)

# E.g.
# View(ns_all_ciwms_list[[3]])
# Should look like first table in sheet '6'.


# From the list of CIWMs, we need to recreate the Total Indicator
# Relevances matrix: the sum of relevance weights across indicators found in
# sheet 'Total Indicator Relevances'.

# A constant of 2 is added to the Total Indicator Relevances in the NatureScot
# calculations. We assume this is to avoid zero divisions. The same constant
# multiplied by 100 will be included when we sum the relevance-weighted
# contributions of quality indicators in the following step (such that indexed
# on itself it always equals 1).

## FUNCTION calc_tir()
calc_tir <- function(all_ciwms_list, tir_constant) {

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + tir_constant

  return(tir)
}

# For Scotland:
made_tir <- calc_tir(all_ciwms_list = ns_all_ciwms_list,
                     tir_constant = ns_tir_constant)

# This should recreate NS sheet "Total Indicator Relevances" (imported above):
all.equal(made_tir, ref_tir)
# It does.


# For any year, a yearly weighted condition contribution matrix (YWCCM) for each
# indicator can be generated by multiplying the CIWM (condition indicator
# weights matrix) by the year's raw condition scores ICC (around 100).
# These objects are not explicitly displayed in the NS spreadsheet; rather they
# are equivalent to the the first table in the numbered
# condition indicator sheets multiplied by the year value in P36 thru 58 on the
# same sheet.

## FUNCTION get_yearly_condition() extracts the indexed condition score for one
# CI in one year:
get_yearly_condition <- function(raw_cis, year_to_get, ci_num, year_list) {

  col_idx <- ci_num

  # Access data by row name (year) and column index
  raw_cond_score <- raw_cis[as.character(year_to_get), col_idx]
  year_one_score <- raw_cis[as.character(year_list[1]), col_idx]

  # Index calculation
  indexed_cond_score <- (raw_cond_score / year_one_score) * 100

  return(as.numeric(indexed_cond_score))
}



# FUNCTION build_all_ywccms() multiplies one year's indexed value by the
# relevance weight matrix (CIWM) for all CIs in the list. Takes all_ciwms_list
# and outputs all_ywccms_list.
build_all_ywccms <- function(raw_cis, year, year_list, ciwms_list) {

  # Iterate through the list of CIWMs
  all_ywccms_list <- lapply(seq_along(ciwms_list), function(ci_num) {

    # Get the indexed condition score for this specific indicator (ci_num)
    ci_this_year <- get_yearly_condition(
      raw_cis = raw_cis,
      year_to_get = year,
      ci_num = ci_num,
      year_list = year_list
    )

    # Safety check but shouldn't be required: ensure NAs don't break the multiplication
    # if(is.na(ci_this_year)) ci_this_year <- 0

    # Multiply condition score by corresponding weighted relevance matrix
    ywccm <- ciwms_list[[ci_num]] * ci_this_year

    return(ywccm)
  })

  # Maintain the names (e.g., ind1, ind2) from the original list
  names(all_ywccms_list) <- names(ciwms_list)

  return(all_ywccms_list)
}


# E.g.
# testit <- build_all_ywccms(raw_cis = ns_ci_score_matrix,
#                            year = 2022,
#                            year_list = ns_year_list,
#                            ciwms_list = ns_all_ciwms_list)
# View(testit[[2]])
# View(testit[[1]])
# remove(testit)
# Values should be the indexed year value of the CI * the weight, in the correct
# cells of the matrix, and that looks correct.


# Next, we will sum together the YWCCMs for all indicators to create the Total
# Yearly Flow of ecosystem services from Scotland's habitats in each year.
# Again this is not explicitly displayed in the spreadsheet.

# FUNCTION build_tyf() takes the list of YWCCMs for a year and divides
# by the Total Indicator Relevances. Note the inclusion of the TIR constant
# term both in the denominator, and in the numerator where it is multiplied by
# 100 to give an indexed term congruent with those of the 'real' CIs:
build_tyf <- function(list_of_ywccms, tir, tir_constant) {

  # Get sum of yearly weighted condition contributions for a year
  sum_ywccms <- Reduce("+", list_of_ywccms)

  # Add the TIR constant * 100 and divide by TIR
  tyf <- (sum_ywccms + (100 * tir_constant)) / tir

  return(tyf)
}



# FUNCTION build_all_tyfs(), for each year in the series, uses build_all_yccms()
# to generate the yearly relevance-weighted contribution for each CI, before
# using build_tyf() to combine these. Outputs a list of TYFs, one for each year
# of the series.
build_all_tyfs <- function(raw_cis, year_list, ciwms_list, tir, tir_constant) {

  # Call the process for every year in the list
  raw_tyf_list <- lapply(year_list, function(yr) {

    # STEP A: Build all the individual indicator matrices for THIS year
    current_year_ywccms <- build_all_ywccms(
      raw_cis = raw_cis,
      year = yr,
      year_list = year_list,
      ciwms_list = ciwms_list
    )

    # STEP B: Sum them and normalize using the updated build_tyf
    tyf <- build_tyf(
      list_of_ywccms = current_year_ywccms,
      tir = tir,
      tir_constant = tir_constant
    )

    return(tyf)
  })

  names(raw_tyf_list) <- year_list
  return(raw_tyf_list)
}




# For Scotland:
scot_tyfs_list <- build_all_tyfs(raw_cis = ns_ci_score_matrix,
                                year_list = ns_year_list,
                                ciwms_list = ns_all_ciwms_list,
                                tir = made_tir,
                                tir_constant = ns_tir_constant)


#### CALCULATE NATURAL CAPITAL ASSETS ####

# FUNCTION build_ncai_matrix builds a matrix of actual natural capital in one
# year as per the year sheets "2000", "2002", etc.:
build_ncai_matrix <- function(tyf, wellbeing_base, habitat_extent, target_year, year_one) {

  # Convert to characters for safe indexing
  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors
  extent_target_vec <- habitat_extent[[target_str]]
  extent_origin_vec <- habitat_extent[[origin_str]]

  # Index the habitat extent values
  extent_index <- (extent_target_vec / extent_origin_vec * 100)
  # extent_index[!is.finite(extent_index)] <- 0 # hopefully redundant?

  # Multiply the wellbeing base by the tyf
  # Ensure they are both treated as matrices for element-wise multiplication
  wb_tyf <- as.matrix(tyf) * as.matrix(wellbeing_base)

  # Apply the extent index across the rows (Habitats)
  # sweep() works perfectly here: MARGIN 1 applies extent_index[i] to every cell in row i
  ncai_matrix <- sweep(
    x = wb_tyf,
    MARGIN = 1,
    STATS = extent_index,
    FUN = "*"
  )

  # Divide by 10,000 as per your sheet calculation
  return(as.data.frame(ncai_matrix / 10000))
}

# FUNCTION build_all_nca_matrices() builds the year sheet for every year in the
# year list:
build_all_ncai_matrices <- function(tyf_list,
                                    wellbeing_base,
                                    habitat_extent,
                                    year_one,
                                    habitat_labels) {

  # Iterate over the names (years) of the tyf_list
  all_ncai <- lapply(names(tyf_list), function(yr) {

    ncai_df <- build_ncai_matrix(
      tyf = tyf_list[[yr]],
      wellbeing_base = wellbeing_base,
      habitat_extent = habitat_extent,
      target_year = yr,
      year_one = year_one
    )
    rownames(ncai_df) <- habitat_labels

    return(ncai_df)
  })

  names(all_ncai) <- names(tyf_list)
  return(all_ncai)
}

made_ncai_year_matrices <- build_all_ncai_matrices(
  tyf_list = scot_tyfs_list,
  wellbeing_base = made_wellbeing_base,
  habitat_extent = ns_habitat_extent,
  year_one = ns_year_list[[1]],
  habitat_labels = ns_all_habitat_labels
)

# In some years we manage to recreate the NatureScot results, but not others.
# We can test the whole series against the values we imported from the
# spreadsheet at the start:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)
}, made_ncai_year_matrices[1:23], ref_all_year_sheets[1:23], SIMPLIFY = FALSE)


# We find errors in years 2019 and 2022:
comparison_results

# After close inspection we are able to pinpoint transcription errors in the
# spreadsheet.
# 1. In year sheet "2022" the cell formula includes multiplication by the
# habitat extent data for year 2021 (Z44, Z45, etc. in sheet "Ecosystem Area")
# instead of 2022 (should be AA44, AA45, etc.)

# In Sheet "66" (CI 32 in the sequence of 38), there is a an error in cell P55.
# This cell should contain a formula to calculate the indexed value of the
# score for the year 2019. Instead it contains the number equal to the indexed
# value of the year 2018 score.

# We have manually corrected the spreadsheet (fixing the erroneous value in
# sheet "66" and the erroneous formulae in sheet "2022":
ns_corrected_sheets_path <- file.path("dev", "ncai_corrected.xlsx")

# We import the corrected version of the Condition Indicator scores
# matrix:
ns_corrected_ci_score_matrix <- read_the_ci_scores(sheet_path = ns_corrected_sheets_path,
                                                   sheet_list = 9:46,
                                                   vector_range = "I36:I58",
                                                   ci_ids = ns_ci_ids)

# And the corrected version of the year sheets:
ns_corrected_all_year_sheets <- lapply(X = ns_year_sheets_ids,
                                       FUN = read_ns_year_sheet,
                                       path = ns_corrected_sheets_path,
                                       service_labels = all_es_labels,
                                       habitat_labels = all_habitat_labels)

rownames(ns_corrected_ci_score_matrix) <- ns_year_list

# Calculate flow again:
scot_corrected_tyfs_list <- build_all_tyfs(raw_cis = ns_corrected_ci_score_matrix,
                                           year_list = ns_year_list,
                                           ciwms_list = ns_all_ciwms_list,
                                           tir = scot_tir,
                                           tir_constant = ns_tir_constant)

# Calculate assets again:
scot_corrected_ncai_year_matrices <- build_all_ncai_matrices(
  tyf_list = scot_corrected_tyfs_list,
  wellbeing_base = ns_wellbeing_base,
  habitat_extent = ns_habitat_extent,
  year_one = ns_year_list[[1]],
  habitat_labels = all_habitat_labels
)

# Compare results again:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)},
  scot_corrected_ncai_year_matrices[1:23],
  ns_corrected_all_year_sheets[1:23],
  SIMPLIFY = FALSE)
comparison_results

# And now we recreate the calculation without discrepancies.


## CALCULATE INDEXED NATURAL CAPITAL ASSETS
# FUNCTION cal_ncai() collects the sum of actual assets, and calculates an
# indexed value and subsequently a smoothed index value. Default settings as
# per NatureScot. Returns a dataframe of indexed values one row per year.
calc_ncai <- function(total_assets_matrix_list,
                      smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                      year_one = names(total_assets_matrix_list)[[1]]) {

  # Get the raw totals.
  yearly_sums <- sapply(total_assets_matrix_list, sum, na.rm = TRUE)

  # These will indexed on year one to give a 'raw' index.
  year_one_val <- yearly_sums[[year_one]]

  # Smoothing is applied, with more recent values given more weight.
  # Define the smoothing function:
  weighted_smooth <- function(window_vec) {
    current_weights <-  tail(smoothing_weights, length(window_vec))
    current_divisor <- sum(current_weights)
    return(sum(window_vec * current_weights) / current_divisor)
  }

  # Build as a dataframe
  indices_df <- data.frame(
    raw_total = yearly_sums,
    raw_index = yearly_sums / year_one_val * 100,
    row.names = names(total_assets_matrix_list)
  ) %>%
    mutate(
      smoothed_index = slide_dbl(
        .x = raw_index,
        .f = weighted_smooth,
        .before = 4,
        .complete = FALSE
      )
    )

  return(indices_df)

}

# To test this:
# Get the overall NCA index set:
scot_overall_index <- calc_ncai(scot_corrected_ncai_year_matrices)

# This can be compared to the first entry (named "overall") in the imported
# ns_index_breakdowns. We need to import the indexes from the corrected version
# of the spreadsheet:
ns_index_breakdowns <- lapply(index_breakdown_ranges, function(rng) {
  read_the_indices(
    indices_range = rng,
    sheet_path = ns_corrected_sheets_path,
    sheet = 73
  )
}) %>%
  setNames(index_breakdown_labels)

test_ns_overall_index <- ns_index_breakdowns[["overall"]] %>%
  setNames(names(scot_overall_index)) %>%
  # NB displayed raw total in NS sheet is divided by 100 so, for comparison:
  mutate(raw_total = raw_total * 100) %>%
  # Also, and we don't know if this is a mistake or not, no raw index is shown
  # for the overall index. Instead the smoothed value is displayed in that
  # column, so let's deselect that column in both for testing:
  select(-raw_index)

test_scot_overall_index <- scot_overall_index %>%
  select(-raw_index)
all.equal(test_scot_overall_index, test_ns_overall_index)
remove(test_scot_overall_index)
remove(test_ns_overall_index)


## Calculating NCAI, broken down by service type:
# FUNCTION calc_ncai_by_st() allows breakdown of the index by groups of
# columns (services) passed in as a list, named by service type:
calc_ncai_by_st <- function(total_assets_matrix_list,
                            es_label_tree_list,
                            ...) {

  lapply(es_label_tree_list, function(subset_labels) {

    filtered_matrix_list <- lapply(total_assets_matrix_list, function(m) {
      m[, subset_labels, drop = FALSE]
    })

    calc_ncai(filtered_matrix_list, ...)
  })
}

# For Scotland, the index broken down by service type is:
scot_index_by_st <- calc_ncai_by_st(
  total_assets_matrix_list = scot_corrected_ncai_year_matrices,
  es_label_tree_list = es_label_tree)

# We can compare with the index breakdowns in the NatureScot sheet:
ns_index_by_st <- ns_index_breakdowns[names(es_label_tree)]

# Are they equal?
all.equal(scot_index_by_st, ns_index_by_st)
# Yes.


## Calculating NCAI, broken down by broad habitats:
# FUNCTION calc_ncai_by_bh() allows breakdown of the index by groups of rows
# (habitats):
calc_ncai_by_bh <- function(total_assets_matrix_list,
                            habitats_label_tree,
                            ...) {

  lapply(habitats_label_tree, function(subset_labels) {

    filtered_matrix_list <- lapply(total_assets_matrix_list, function(m) {
      m[subset_labels, , drop = FALSE]
    })

    calc_ncai(filtered_matrix_list, ...)
  })
}

# For Scotland, the index broken down by broad habitat is:
scot_index_by_bh <- calc_ncai_by_bh(
  total_assets_matrix_list = scot_corrected_ncai_year_matrices,
  habitats_label_tree = habitats_label_tree)

# We can compare with the index breakdowns in the NatureScot sheet.
# In NatureScot's spreadsheet, breakdowns of the NCAI are calculated for a
# selection of the broad habitats (H unvegetated and K montane groups are
# not calculated/reported), so we make a list of the broad habitats in use in
# the NatureScot sheet
# We create a list of all breakdowns in use:
ns_bh_breakdowns <- c(broad_habitat_labels[c(1:6, 8)])
# And get that subset from the imports:
ns_index_by_bh <- ns_index_breakdowns[ns_bh_breakdowns]
# We take the similar subset from those we just calculated:
scot_breakdowns_to_test <- scot_index_by_bh[c(1:6, 8)]

# Are they equal?
all.equal(scot_breakdowns_to_test, ns_index_by_bh)
# Yes.


#### PLOT INDICES ####

# We can plot the main index.
# Datasets for plotting...
# Just the overall index:
main_index_for_plot <- scot_overall_index %>%
  rownames_to_column(var = "year") %>%
  mutate(
    year = as.numeric(year),
    breakdown = "overall")

# Breakdown by ecosystem service type + main trend
by_st_for_plot <- scot_index_by_st[es_type_labels] %>%
  lapply(rownames_to_column, var = "year") %>%
  bind_rows(.id = "breakdown") %>%
  mutate(year = as.numeric(year)) %>%
  bind_rows(main_index_for_plot)

# Breakdown by broad habitat + main trend
# Use the subsetted data with only the broad habitats graphed by NS
by_bh_for_plot <- scot_breakdowns_to_test[ns_bh_breakdowns] %>%
  lapply(rownames_to_column, var = "year") %>%
  bind_rows(.id = "breakdown") %>%
  mutate(year = as.numeric(year)) %>%
  bind_rows(main_index_for_plot)

# The Overall Index
# (NatureScot graph plots the unsmoothed values)
ggplot(main_index_for_plot, aes(x = year, y = raw_index)) +
  # Line:
  geom_line(color = "#003366", linewidth = 1.2) +
  # Recreate similar scale:
  scale_y_continuous(
    limits = c(90, 110),
    breaks = seq(90, 110, by = 2),
    expand = c(0, 0)
  ) +
  # Similar X axis labelling:
  scale_x_continuous(
    breaks = seq(2000, 2022, by = 3),
    minor_breaks = seq(2000, 2022, by = 1),
    expand = c(0, 0.5)
  ) +
  # Labelling:
  labs(title = "Overall Natural Capital Asset Index 2000 to 2022",
       x = NULL,
       y = "Index (Year 2000 = 100)") +
  # Theming:
  theme() +
  theme(
    panel.grid.major.x = element_blank(),     # No vertical lines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"), # Light horizontal lines
    axis.line.x = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 11),
    panel.background = element_rect(fill = "white", color = NA)
  )
# Save it:
ggsave(
  filename = file.path("dev", "ncai_overall_trend.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 6,               # Height in inches
  dpi = 300                 # High resolution for reports
)


# The index by service type with main index for reference:
ggplot(by_st_for_plot, aes(x = year, y = raw_index, color = breakdown)) +
  # Lines
  geom_line(linewidth = 1.1) +

  # Diamond markers on overall line
  geom_point(data = filter(by_st_for_plot, breakdown == "overall"),
             shape = 18, size = 3) +

  # Colours roughly in line with NatureScot
  scale_color_manual(values = c(
    "overall" = "#003366",                   # Dark Blue
    "provisioning" = "#660033",              # Maroon/Purple
    "regulation_and_maintenance" = "#FF6600", # Orange
    "cultural" = "#339999"                   # Teal
  ),
  labels = c("Overall", "Provisioning", "Regulation & maintenance", "Cultural")) +

  # Adjust the axes
  scale_y_continuous(
    limits = c(90, 110),
    breaks = seq(90, 110, by = 5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2022, by = 3),
    minor_breaks = seq(2000, 2022, by = 1),
    expand = c(0, 0.5)
  ) +

  # Labels
  labs(
    title = "NCAI 2000 to 2022 by type of ecosystem service",
    x = NULL,
    y = "Index (Year 2000 = 100)",
    color = NULL
  ) +
  # Theming
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"), # Subtle horizontal lines
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "black")
  )
# Save it:
ggsave(
  filename = file.path("dev", "ncai_by_ecosystem_service_type.png"),
  plot = last_plot(),       # Optional: defaults to the last plot displayed
  width = 10,               # Width in inches
  height = 6,               # Height in inches
  dpi = 300                 # High resolution for reports
)


# The index by broad habitat with main index for reference:
ggplot(by_bh_for_plot, aes(x = year, y = raw_index, color = breakdown)) +
  # Lines
  geom_line(linewidth = 1.1) +

  # Diamond markers on overall line
  geom_point(data = filter(by_bh_for_plot, breakdown == "overall"),
             shape = 18, size = 3) +

  # Colours roughly in line with NatureScot
  scale_color_manual(
    values = c(
      "woodland"      = "#339999",  # Teal/Green
      "freshwater"    = "#404040",  # Dark Grey
      "coastal"       = "#FF6600",  # Orange
      "overall"       = "#003366",  # Dark Blue
      "grasslands"    = "#660033",  # Maroon
      "moorland"      = "#9980CC",  # Purple/Lavender
      "wetlands"      = "#FFCC00",  # Yellow
      "cropland"      = "#0070C0"   # Bright Blue
    ),
    labels = c(
      "woodland"      = "Woodland",
      "freshwater"    = "Inland surface waters",
      "coastal"       = "Coastal",
      "overall"       = "Overall trend",
      "grasslands"    = "Grasslands",
      "moorland"      = "Heathland",
      "wetlands"      = "Mires, bogs, fens",
      "cropland"      = "Agriculture & cultivated"
    )
  ) +

  # Adjust the axes
  scale_y_continuous(
    limits = c(85, 120),
    breaks = seq(85, 120, by = 5),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2021, by = 3),
    expand = c(0, 0.5)
  ) +

  # Labels
  labs(
    title = "NCAI 2000 to 2022 by type of habitat",
    x = NULL,
    y = "Index (Year 2000 = 100)",
    color = NULL
  ) +
  # Theming
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"), # Subtle horizontal lines
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "black")
  )
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


