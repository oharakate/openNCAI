## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


# SETUP ####
# install.packages("slider")

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)

# Scotland data for replication:
# Existing bases (Scotland)
# Use read_xlsx to bring these in without any unintentional rounding now.
# Index of sheets in the NatureScot spreadsheet:
ns_sheets_index <- excel_sheets("dev/ncai.xlsx")
ns_sheets_index
# Wellbeing base
ns_wellbeing_base <- read_xlsx("dev/ncai.xlsx",
                sheet = 7,
                range = "F4:AG34",
                col_names = FALSE,
                col_types = "numeric",
                trim_ws = TRUE,
                .name_repair = "minimal"
                ) %>%
  as.data.frame()
# Ecosystem service potential base
ns_espb <- read_xlsx("dev/ncai.xlsx",
                  sheet = 6,
                  range = "F4:AG34",
                  col_names = FALSE,
                  col_types = "numeric",
                  trim_ws = TRUE,
                  .name_repair = "minimal") %>%
  as.data.frame()
# Ecosystem service providing potential per SPU matrix:
ns_esppu <- read_xlsx("dev/ncai.xlsx",
                  sheet = 3,
                  range = "F4:AG34",
                  col_names = FALSE,
                  col_types = "numeric",
                  trim_ws = TRUE,
                  .name_repair = "minimal") %>%
  as.data.frame()
# Habitat extent data years to 2022 (Scotland)
# FIX change to bring in here using the function
# This CSV was processed automatically in fns_import_extent_data:
ns_habitat_extent <- read_csv(file.path("dev", "scot_extent_data_automated.csv"),
               col_names = FALSE,
               show_col_types = FALSE) %>%
  as.data.frame()
# Indicator directory
# FIX bring in automatically!
# This one was manually created in excel, but shouldn't pose a rounding
# problem as the values are exact:
ns_indd <- read.csv("dev/scot_indicator_directory.csv", header = TRUE) %>%
  as.data.frame()


# ES Potential ('Scotland weights')
# Chris has typed these manually below, but here they are as csv,
# but we should
# FIX this so that we just read them from the spreadsheet.
ns_st_importance_weights <- read.csv("dev/scotland_weight_raw_service_sections.csv", header = FALSE)
# main_weights <- c(10,20,10)
ns_prov_importance_weights <- read.csv("dev/scotland_weights_raw_provisioning.csv", header = FALSE)
# provisioning_weights <- c(20,15,9,9,20,13,13,7,11,12,1,0)
ns_regu_importance_weights <- read.csv("dev/scotland_weights_raw_regulationmaintenance.csv", header = FALSE)
# regulationandmaintenance_weights <- c(10,10,12,7,7,10,10,12,14,8,20)
ns_cult_importance_weights <- read.csv("dev/scotland_weights_raw_cultural.csv", header = FALSE)
# cultural_weights <- c(20,20,20,20,20)
# We should also make a list of the three within-st sets.

# Labels for data:
st_labels <- c("provisioning", "regulation_and_maintenance", "cultural")
short_main_labels <- c("prov", "regu", "cult")
provisioning_labels <- c("cultivated_crops",
                         "reared_animals",
                         "wild_animals_plants_algae",
                         "aquaculture_animals_plants_algae",
                         "water_drinking",
                         "materials_direct_animals_plants_algae",
                         "materials_agricultural_animals_plants_algae",
                         "genetic_material",
                         "water_non-drinking",
                         "plant_energy",
                         "animal_energy",
                         "animal_mechanical_energy")
regulationandmaintenance_labels <- c("waste_mediation_biota",
                                     "waste_mediation_ecosystem",
                                     "erosion_mediation",
                                     "flood_protection",
                                     "storm_protection",
                                     "pollination_dispersal",
                                     "nursery_population_habitat",
                                     "pest_disease_control",
                                     "soil_formation_composition",
                                     "water_chemistry",
                                     "climate")
cultural_labels <- c("physical_experience",
                     "heritage_educational",
                     "aesthetic_entertainment",
                     "symbolic_sacred_religious",
                     "existence_bequest")

habitat_codes <- c("b1", "b2", "b3", "c", "d1", "d2", "d4", "d5",
                "e1", "e2", "e4","e5", "e7",
                "f2","f3","f4","f9",
                "g1","g3","g4","g5","g6",
                "h2", "h3","i1","i2",
                "j1","j2","j3","j4","k")

# The range of years in the Scotland 23 extent data:
year_labels <- as.character(2000:2022)

# Apply labels
colnames(ns_espb) <- colnames(ns_esppu) <- colnames(ns_wellbeing_base) <-
  all_service_labels <-
  c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)

rownames(ns_espb) <- rownames(ns_esppu) <- rownames(ns_wellbeing_base) <-
  rownames(ns_habitat_extent) <- habitat_codes

colnames(ns_habitat_extent) <- year_labels

####



#### CALCULATING NCAI ####

# RECREATING THE ES POTENTIAL BASE

# espu as weight - in the NatureScot spreadhsheet, ESPPU contains scores out of
# 5 on the potential of a service-providing unit to deliver its potential. So
# we will divide everything in that by max score 5 to get an ESPPU weight.

# EXCEPT in the case of the red cells, which are multiplied by 5.
# We need a matrix which records the divisor for each habitat/service type
# combination.

## NatureScot function make_custom_divisor_matrix() builds a matrix of
# divisors where some are changed to a new value.

# For Nature Scot's custom divisor matrix, these paired character
# vectors identify all habitat/service-type combinations where the divisor is
# changed:
ns_habitats_to_adjust = c(rep("b1",7), rep("b2",5), rep("b3",5), "d1",
                       rep("i2",6), rep("j1",5), rep("j2",5))
ns_services_to_adjust = c("erosion_mediation", "soil_formation_composition",
                       cultural_labels,
                       cultural_labels,
                       cultural_labels,
                       "climate",
                       "climate",
                       cultural_labels,
                       cultural_labels,
                       cultural_labels)

# Make the matrix of ScotNCAI adjustments to the weights:
ns_custom_divisor_matrix <- make_custom_divisor_matrix(
  habitat_codes = habitat_codes,
  all_service_labels = all_service_labels,
  habitats_to_adjust = ns_habitats_to_adjust,
  services_to_adjust = ns_services_to_adjust,
  usual_divisor = 5,
  custom_divisor = 1
)



## FUNCTION esppu_scores_to_weights()
# Takes dataframe object of ESSPU scores (matrix habitats/ecosystem services)
# and converts it to weights by dividing by a common denominator.

esppu_scores_to_weights <- function(
    esppu, # dataframe habitat type / ecosystem service
    divisor = 5, # divisor for calculating weights from scores
    custom_divisor_matrix = NULL # dataframe habitat type / ecosystem service
                                # containing custom divisors
    ) {

  # Divide all scores by universal divisor if no customisations
  if (is.null(custom_divisor_matrix)) {
    esppu_aw <- esppu / divisor
  } else {
  # Or use custom divisor per habitat/ecosystem service combination
    if (!all(dim(esppu) == dim(custom_divisor_matrix))) {
      stop("Dimensions of esppu and custom_divisor_matrix must match.")
    }
    esppu_aw  <- esppu / custom_divisor_matrix
  }

  return(esppu_aw)
}

# For the Scottish data:
scot_esppu_weights <- esppu_scores_to_weights(
  esppu = ns_esppu,
  divisor = 5,
  custom_divisor_matrix = ns_custom_divisor_matrix)


# To calculate ESPB (ecosystem service potential base) for Scotland,
# we need Scotland's year one data.


## FUNCTION calc_espb() calculates the ecosystem service potential base. It
# takes the habitat extent data, year list and ESPPU weights and multiplies each
# habitat/service combination by the year one area of that habitat.
calc_espb <- function(habitat_extent, esppu_weights, year_list, habitat_labels) {

  year_one <- year_list[1]
  # Pull the vector for original year:
  origin_year_vec <- habitat_extent %>%
    pull(year_one)
  # These habitat extent values are multiplied by their esppu weightings:
  espb <- sweep(
    x = esppu_weights,
    MARGIN = 1,
    STATS = origin_year_vec,
    FUN = "*"
  )
  rownames(espb) <- habitat_labels

  return(espb)
}

# For Scotland:
scot_espb = calc_espb(habitat_extent = ns_habitat_extent,
                      esppu_weights = scot_esppu_weights,
                      year_list = year_labels,
                      habitat_labels = habitat_codes)

# Does the calculated scot_espb match the published ns_espb?
# Yes:
all.equal(ns_espb, scot_espb)

## RECREATING THE WELLBEING BASE
## Next step is to recreate the wellbeing base.

# # FUNCTION imp_rtw_between()
# Gets between-service-provision-type IMPORTANCE weights from a df of raw
# scores.
# Output is used in imp_rtw_within.

# Convert sections of raw ratings to actual weights:
# Guidance from excel sheet:
# Get service section weights from a vector of scores
# ("Step 1: ecosystem service section. The most important of the <list> is
# assigned a value of 20, and the other <remainder> are assigned a value
# (between 0 and 20) in terms of their relative importance.
# 1. Provisioning (1.1 thru 1.12)
# 2. Regulation and maintenance (2.1 thru 2.11)
# 3. Cultural services (3.1 through 3.5
imp_rtw_between <- function(between_scores) {
  # Scores is a vector of scores 1,2 3
  between_weights <- between_scores / sum(between_scores) * 100
  # Return a df which can be indexed [,1] [,2] [,3]
  return(between_weights)
}


## FUNCTION imp_rtw_within()
# Gets within-service-type IMPORTANCE weights from a df of raw scores, using
# between weights output from imp_rtw_between()
# Used in calc_imp_weights() below
imp_rtw_within <- function(within_scores, between_weights, index) {
  # Takes index 1-3 for the appropriate section.
  # Should improve this to make that list of indices soft maybe.
  # Maybe could add a section index column to the df holding the sets of
  # within weights?
  within_weights  <- within_scores / sum(within_scores) * between_weights[index, 1]

  return(within_weights)
}


## FUNCTION calc_imp_weights()
# Calculates importance weights, using within and between weights.
# Loops through the list of ecosystem service types, calculating importance
# weights and returning a list of weight subset objects.

# Requires the vector of between-service-type scores, and a list of the
# within-service-type-score objects.
calc_imp_weights <- function (between_scores, within_scores_list) {

  # Calculate the between weights
  b_weights <- imp_rtw_between(between_scores)

  # Initialise service type group number
  st_num <- 0

  # Initialise list of weight subsets
  ww_subset_list <- list()

  for (i in within_scores_list) {

    # Increment service type group number
    st_num <-  st_num + 1
    ww_subset_name <- paste0("ww_subset_", st_num)

    # Calculate within weights for service type
    ww_subset <- imp_rtw_within(within_scores = within_scores_list[[st_num]],
                                between_weights = b_weights,
                                index = st_num)
    assign(ww_subset_name, ww_subset, envir = .GlobalEnv)

    # Add the ww subset to the list thereof
    ww_subset_list[[ww_subset_name]] <- ww_subset

  }

  return(ww_subset_list)

}

# eswr_scot_sections are the scottish between-ecosystem-service-type raw scores
# Make a list of the within-ecosystem_service_type scores
scot_within_score_list <- list(eswr_prov, eswr_regu, eswr_cult)

scot_ww_list <- calc_imp_weights(eswr_scot_sections, scot_within_score_list)

## FUNCTION bind_imp_weights()
# Rejoins within-service-type weights back into one weight vector, applying
# between-service-type weights.

# Require list of importance within weight vectors, output from imp_rtw_within()
# and list of all the service labels

bind_imp_weights <- function(ww_list, all_service_label_list) {

  # Row bind the subsets of weights
  long_weights <- dplyr::bind_rows(ww_list)

  if(nrow(long_weights) != length(all_service_label_list)) {
    stop("The number of rows in the weights (", nrow(long_weights),
         ") does not match the number of labels (", length(all_service_label_list), ").")
  }

  # Label rows with the service type subsets of service labels
  rownames(long_weights) <- all_service_label_list
  colnames(long_weights) <- ("weight")

  # Pivot wider to make one row df, services as cols
  wide_joined_weights <- as.data.frame(t(long_weights))

  return(wide_joined_weights)

}

# Rejoin the within-weight objects and pivot wide:
scot_iw_within <- bind_imp_weights(ww_list = scot_ww_list,
                                   all_service_label_list = all_service_labels)


## FUCNTION calc_wb() takes the espb (ES potential per habitat/service type
# combo) and expresses each cell as a proportion of
# the total potential for that service type across all habitats (colSums).
# Next it multiplies in iw the importance weights (result of between and within
# service-type weighting process above)).
# Returns the wellbeing base which is a matrix of habitat/service type.
calc_wb <- function(espb, # ES potential, a matrix habitat/service type
                    iw # Importance weights, a vector (wide df) by service type
                    ) {

  # Express ESPB as proportion of habitat total contribution
  espb_totals <- colSums(espb)
  espb_as_prop <- sweep(
    x = espb,
    MARGIN = 2,
    STATS = espb_totals,
    FUN = "/"
  )

  # Multiply by within-service-type importance weights
  wb <- sweep(
    x = espb_as_prop,
    MARGIN = 2,
    STATS = as.numeric(iw),
    FUN = "*"
  )

  # Multiply by 100
  wb <- wb * 100

  return(wb)

}

# Calculate Scotland's Well-being Base:
scot_wb <- calc_wb(scot_espb, scot_iw_within)

all.equal(wb, scot_wb, check.attributes = FALSE)
# TRUE!



################################
## UP TO HERE IS CORRECT, THEN SOMETHING IS WRONG


# OK, so up to here we have been able to recreate the potential base and the
# wellbeing base from year 1 (2000) Scotland extent data and the two sheets
# which contain the ecosystem service potential per SPU (sheet 3) and the
# between and within service type 'importance to Scotland' weights (sheet 4).

# Next we are going to need the condition indicators data and their respective
# weight matrices (actually we want them as relevance matrices, to use with
# the weights from the indicator directory indd).

# We have automatically processed these for the NatureScot spreadsheet:

# 1. fns_bring_in_ci_raw_scores() was used to get each set of raw scores (after
# any custom adjustments and inter/extrapolating) and put these into a matrix
# of shape year/CI. These are found in scot_year_ci_matrix_automated.csv.

# We need a wee helper function to add the year column to the raw CIs matrix:
## FUNCTION add_years_to_ci_matrix
add_years_to_ci_matrix <- function(raw_cis, year_list) {

  raw_cis_with_years <- as.data.frame(raw_cis)

  raw_cis_with_years <- raw_cis %>%
    as.data.frame() %>%
    mutate(year = year_list, .before = 1)

  return(raw_cis_with_years)

}

label_ci_matrix <- function(raw_cis_with_years, year_list) {

  colnames(raw_cis_with_years) <- c("year",
                                    paste0("ind",
                                           1:(ncol(raw_cis_with_years)-1)))

  return(raw_cis_with_years)
}

# Do this to Scot CIs:
scot_raw_ci_score_matrix <- read_csv(
  file.path("dev", "scot_year_ci_matrix_automated.csv"),
  show_col_types = FALSE
) %>%
  add_years_to_ci_matrix(scot_year_list) %>%
  label_ci_matrix()

# Check it has years, columns called ind#, 23 rows, 38 indicators.
# View(scot_raw_ci_score_matrix)
sum(is.na(scot_raw_ci_score_matrix))
# It should contain scores from I36 downwards in the numbered indicator sheets.


# We use this function to convert the raw scores to indexed values (around 100):
#### THINK THIS IS NOT REQUIRED. COMMENT OUT ON 23-12-2025. ####
## FUNCTION index_scores() converts matrix of year/ci raw scores to year/ci
## indexed scores. Returns matrix of indices. Requires the indd to get the
# number of CIs we are working with.
# index_scores <- function(score_matrix, indd, year_list) {
#
#   n_cis <- nrow(indd)
#
#   scorecol_names <- paste0("stbi", 1:n_cis)
#   names(score_matrix) <- scorecol_names
#
#   working_matrix <- score_matrix
#
#   for (i in 1:n_cis) {
#     score_name <- paste0("stbi", i)
#     indic_name <- paste0("ind", i)
#
#     y1score <- score_matrix[[score_name]][1]
#
#     working_matrix[[indic_name]] <- (working_matrix[[score_name]] / y1score) * 100
#
#   }
#   index_matrix <- working_matrix %>%
#     select(starts_with("ind")) %>%
#     mutate(year = year_list) %>%  # Add the years here
#     relocate(year)
#
#   return(index_matrix)
# }


# Convert matrix of Scottish year / raw CI scores to indexed values:
# Number of CIs is the rows in the indicator directory.
# scot_cis_indexed <- index_scores(scot_stbi_matrix, scot_indd, scot_year_list)
# head(scot_cis_indexed)
#### CONTINUE ####

# 2. fns_bring_in_cirms() was used to harvest a binary CI relevance matrix in
# shape habitat/ecosystem service for each CI and save these as csv in the
# folder 'cirms'. They are regularly named to facilitate processing with
# functions below.

# 3. The indicator directory is in scot_indicator_directory.csv
# It was assigned to indd above.
# Consider writing a NatureScot function to bring this in with readxl.

## CALCULATING THE WEIGHTED INDICATORS MATRICES
# For each indicator, the relevance matrix needs to be multiplied by the
# ecosystem-service-type weight for that indicator, as recorded in indd.

## FUNCTION get_cirm()
# Takes a path to a folder containing CIRM CSVs and the number of CI to process.
# Opens the CIRM and returns a regularly named/numbered object cirm#. Can take
# the name stub but default value is "cirm".
get_cirm <- function (folder_path, ci_num, csv_stub = "cirm") {

  cirm_file_name <- paste0(paste0(csv_stub, ci_num), ".csv")
  cirm_object <- read_csv(file.path(folder_path, cirm_file_name),
                          col_names = FALSE,
                          show_col_types = FALSE)

  return(cirm_object)

}


## FUNCTION build_ciwm
# This will take a CIRM object named cirm# where the # is the number of the CI.
# Also requires the list of service types and a list of label sets for each
# subtype.

build_ciwm <- function(ci_num,
                       cirm_object,
                       st_list,
                       label_subsets_list,
                       indd = indd) {

  # Determine number of service types
  n_st <- length(st_list)

  # Use a list to store the weighted sub-matrices
  ciwm_parts <- list()

  # 3. Iterate through service types
  for (i in 1:n_st) {
    # Get the column labels for this subset (e.g., from your list)
    current_labels <- label_subsets_list[[i]]

    # Get the weight from the indicator directory (indd)
    wcol_name <- paste0("st", i, "_weight")
    weight <- as.numeric(indd[ci_num, wcol_name])

    # Extract columns from the CIRM and multiply by weight
    # We use [ , current_labels] to subset columns
    sub_ciwm <- cirm_object[, current_labels, drop = FALSE] * weight

    # Store in our list
    ciwm_parts[[i]] <- sub_ciwm
  }

  # Join the subsets back together (Left-to-Right)
  # dplyr::bind_cols is faster and cleaner than Reduce(cbind, ...)
  final_ciwm <- dplyr::bind_cols(ciwm_parts)

  return(final_ciwm)
}



## FUNCTION to process all indicators
# Wraps the two above and produces a list of ciwm objects, equivalent to the
# tables in sheets '2' - '104'.
build_all_ciwms <- function(cirm_csvs_dir,
                            csv_name_stub,
                            all_service_labels,
                            label_subsets_list,
                            st_list,
                            indd = indd) {

  # Initialise list of ciwm objects
  all_ciwms <- list()

  ci_list <- 1:nrow(indd)

  for (i in ci_list) {

    # Load in relevance matrix
    temp_cirm <- get_cirm(cirm_csvs_dir, i, csv_name_stub)

    # Add colnames - may not be necessary
    colnames(temp_cirm) <- all_service_labels

    # Build all ciwms and add to the list
    all_ciwms[[i]] <- build_ciwm(
      ci_num = i,
      cirm_object = temp_cirm,
      st_list = st_list,
      label_subsets_list = label_subsets_list,
      indd = indd
    )

    # Report progress
    message(paste("Indicator", i, "weighted and added to list."))
  }

  return(all_ciwms) # returns a list of ciwm matrix objects

}


# For Scotland:
scot_cirms_dir <- file.path("dev", "cirms")
scot_label_subsets_list <- list(provisioning_labels,
                             regulationandmaintenance_labels,
                             cultural_labels)

all_ciwms_list <- build_all_ciwms(scot_cirms_dir,
                  "scot_cirm",
                  all_service_labels,
                  scot_label_subsets_list,
                  st_list = st_labels,
                  indd = indd)
# E.g.
View(all_ciwms_list[[3]])
# Should look like first table in sheet '6' and it does.


# Still trying to solve exactly how these come together.
# NatureScot use a matching matrix full of 2s (sheet 'a1') in their calculations.
# I think this is just to avoid zero divisions.
# Coding this as an add-on which for recreating Scot NCAI will be 2.

# Looking at e.g. sheet '2000', this looks like it should hold the total
# relevance-weighted contributions per habitat/service for that year.
# The formula does:
# Multiply the indexed condition indicator that year (calculate from raw_cis)
# by the relevance weight for that habitat service (fetch from correct ciwm
# in all_ciwms_list),
# And take the sum of these.
# Divide that by the total indicator relevance per habitat/servce (tir matrix).
# (Note that the add-on 2 is summed into both numerator and denominator here).
# Multiply the result by the wellbeing base values,
# Multiply the result by the ecosystem area (vector by habitat) across the way.
# Divide the result by 10000. CHECK SWEEP IS ON RIGHT AXIS.

# The adding of 2 to the total relevance would be consequential. We can try it
# with and without and see if either works to recreate the numbers.

# From the list of CIWMs, we should be able to recreate the Total Indicator
# Relevances matrix. That's the sum of relevance weights across indicators,
# plus the addon, found in sheet 'Total Indicator Relevances'.
# Call it TIR.
# Make this customisable with the added 2.

## FUNCTION calc_tir()
calc_tir <- function(all_ciwms_list, addon) {

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + addon

  return(tir)
}

# With the 2:
scot_tir_with2 <- calc_tir(all_ciwms_list, addon = 2)
View(scot_tir_with2)

# Without the 2:
# scot_tir_no2 <- calc_tir(all_ciwms_list, addon = 0)
# View(scot_tir_no2)

# Get TIR from the original sheet and compare:
ns_sheets_index
# Use sheet 75:
ncai_tir_auto <- readxl::read_excel(
  path = "dev/ncai.xlsx",
  sheet = 74,
  range = "F4:AG34",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
)
# Check this for NAs
sum(is.na(ncai_tir_auto))
# Are they same?
all.equal(as.data.frame(ncai_tir_auto),
          scot_tir_with2,
          check.attributes = FALSE)
# Yes, so we should keep the add-on function.
# Later, this needs to be involved in the division of weighted condition
# contributions by the total indicator relevances.

# Let's try again to work out how to calc the final index.

# For any year, I think NOW the yearly condition matrix YCM can be generated
# by multiplying the CIWM (condition indicator weights matrix) by the year's
# RAW condition scores ICC (around 100).

# Moving code to add years and indicator labels to the raw CIS matrix further up



# UPDATING THIS TO GET THE INDEXED VALUE AS PER SPREADSHEET
# First extract the indexed condition score for the right CI for the right year.
# Because we do the indexing here, we don't need that commented out function
# to index the raw ci scores above.
## FUNCTION get_yearly_condition()
get_yearly_condition <- function(raw_cis, year_to_get, ci_num, year_list) {
  # Use column index instead of name to be absolutely sure
  # Year is col 1, so ind1 is col 2, ind2 is col 3...
  col_idx <- ci_num + 1

  row_idx <- which(as.character(raw_cis[, 1]) == as.character(year_to_get))
  y1_idx  <- which(as.character(raw_cis[, 1]) == as.character(year_list[1]))

  raw_cond_score <- raw_cis[row_idx, col_idx]
  year_one_score <- raw_cis[y1_idx, col_idx]

  # Index on year one
  indexed_cond_score <- (raw_cond_score / year_one_score) * 100

  return(as.numeric(indexed_cond_score))
}

# E.g.
# testit <- get_yearly_condition(raw_cis = scot_raw_ci_score_matrix,
#                                year_to_get = 2003,
#                                ci_num = 1,
#                                year_list = scot_year_list)
# testit # Compare to spreadsheet (sheet '2' P39) - correct
# remove(testit)


# Next we build a Yearly Weighted CI contribution YWCCM
## FUNCTION
build_ywccm <- function(ci_num, raw_cis, year, year_list, ciwms_list) {

  # Get this year's indexed condition score
  ci_this_year <- get_yearly_condition(
    raw_cis = raw_cis,
    year_to_get = year,
    ci_num = ci_num,
    year_list = year_list
  )

  # Belt and braces check for NAs - prob not essential
  if(is.na(ci_this_year)) ci_this_year <- 0

  # Multiply the indexed score by its weight matrix.
  ywccm <- ciwms_list[[ci_num]] * ci_this_year

  # Returns a matrix with the yearly indexed CI * its weight in each relevant
  # habitat/service cell.
  return(ywccm)

}

# E.g.
# testit <- build_ywccm(ci_num = 2,
#                       raw_cis = scot_raw_ci_score_matrix,
#                       year = 2001,
#                       year_list = scot_year_list,
#                       ciwms_list = all_ciwms_list)
# View(testit)
# remove(testit)
# Values should be the indexed year value of the CI * the weight, in the correct
# cells of the matrix, and that looks correct.

# IMPORTANT
# Here I think is where I went wrong before.
# To do it as per the spreadsheet, multiply the indexed score for the CI/year
# by the CI's weight matrix.
# Add all together and also the add-on.
# Divide by the sum of the relevances for all CIs which is equivalent to the TIR.
# Remember the TIR already has the add-on added.

# The summed yearly weighted contributions we are going to call the
# Total Yearly Condition matrix (tyc)

## FUNCTION build_tyc adds all the CI matrices for that year on top of each
# other, then divides by the total indicator relevances.
build_tyc <- function(raw_cis, target_year, year_list, ciwms_list, tir, addon) {

  # Create indices
  ci_indices <- seq_along(ciwms_list)

  # Create a list to store each YWCCM
  list_of_ywccms <- lapply(ci_indices, function(i) {
    build_ywccm(
      ci_num = i,
      raw_cis = raw_cis,
      year = target_year,
      year_list = year_list,
      ciwms_list = ciwms_list
    )
  })

  # Get sum of yearly weighted condition contributions
  sum_ywccms <- Reduce("+", list_of_ywccms)

  # Divide by the TIR
  # The addon value (from 'a1' sheet) should be included in the numerator
  # (it's already in the denominator TIR). I think it needs to be multiplied
  # by its dummy indexed condition score which is always 100.
  tyc <- (sum_ywccms + (100 * addon)) / tir

  # # Trying this again without that
  # tyc <- (sum_ywccms) / tir

  return(tyc)

}

# THIS PART MAY NOT BE NEEDED!
# FUNCTION build_indexed_tyc
# Takes the raw_tyc and indexes it (around 1) on the year_one raw TYC.
build_indexed_tyc <- function (target_rtyc, year_one_rtyc) {

  # Convert to matrices
  t_mat  <- as.matrix(target_rtyc)
  y1_mat <- as.matrix(year_one_rtyc)

  indexed_tyc <- (t_mat / y1_mat) * 100

  # Deal with 0/0 divisions
  indexed_tyc[!is.finite(indexed_tyc)] <- 0

  return(indexed_tyc)

}


# NOW STOP AND CONSIDER THE WAY FORWARD



# For Scotland, the year 2000 total contributions:
scot_2000_tyc <- build_tyc(
  raw_cis = scot_raw_ci_score_matrix,
  target_year = 2000,
  year_list = scot_year_list,
  ciwms_list = all_ciwms_list,
  tir = scot_tir_with2,
  addon = 2)
scot_2000_ityc <- build_indexed_tyc(scot_2000_tyc, scot_2000_tyc)
# View(scot_year_2000_tyc)

# And those for 2001:
scot_2001_tyc <- build_tyc(
  raw_cis = scot_raw_ci_score_matrix, target_year = 2001,
  year_list = scot_year_list,
  ciwms_list = all_ciwms_list,
  tir = scot_tir_with2,
  addon = 2)
scot_2001_ityc <- build_indexed_tyc(scot_2001_tyc, scot_2000_tyc)

# Get 2022 for testing
scot_2022_tyc <- build_tyc(
  raw_cis = scot_raw_ci_score_matrix,
  target_year = 2022,
  year_list = scot_year_list,
  ciwms_list = all_ciwms_list,
  tir = scot_tir_with2,
  addon = 2)
scot_2022_ityc <- build_indexed_tyc(scot_2022_tyc, scot_2000_tyc)


## CALCULATE THE INDEX FOR YEAR 2000

# If all this has worked well, then the TYCM is :
# multiplied by the WB
# multiplied by indexed ED (ED this year/ED year one * 100)
# and divided by 10,000 to give a number around 100.


# FUNCTION build_ncai_matrix
# takes the wb and tyc and multiplies with the indexed extent data for the
  # year in question
build_ncai_matrix <- function(wb, ityc, ed, target_year, year_one) {

  # Convert to characters for safe column indexing
  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors from the ed matrix/df
  ed_target_vec <- ed[[target_str]]
  ed_origin_vec <- ed[[origin_str]]

  # Index the habitat extent values.
  # This is seen in the year sheet calculations as the indexed value (drawn
  # from the lower table in ecosystem area) of ecosystem area (extent) is used.
  extent_index <- (ed_target_vec / ed_origin_vec * 100)
  extent_index[!is.finite(extent_index)] <- 0
  # CHECK HERE
  # Sheet A1 is the sheet full of 2s, which was the addon used above.

  # Multiply the wb by the ityc
  wb_tyc <- as.matrix(ityc) * as.matrix(wb)

  # And multiply in the indexed habitat extent values for that year
  ncai_matrix <- sweep(
    x = wb_tyc,
    MARGIN = 1,        # Apply to Rows
    STATS = extent_index,
    FUN = "*"
  )

  return(as.data.frame(ncai_matrix/10000))
}

matrix_2000 <- build_ncai_matrix(
  wb = wb,
  ityc = scot_2000_ityc,
  ed = ed,
  target_year = 2000,
  year_one = 2000)
# View(matrix_2000)

# THIS NOW LOOKS RIGHT
# but I'm not sure I see why we had to index the tyc.
# We need to check if it works for further years like that.
# First check if this really is right.
# Get some year sheets for checking:
ns_sheets_index
sheet2000 <- readxl::read_excel(
  path = "dev/ncai.xlsx",
  sheet = 50,
  range = "F4:AG34",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
)
sheet2001 <- readxl::read_excel(
  path = "dev/ncai.xlsx",
  sheet = 51,
  range = "F4:AG34",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
)
sheet2022 <- readxl::read_excel(
  path = "dev/ncai.xlsx",
  sheet = 72,
  range = "F4:AG34",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
)

# Check if we calculated 2000 exactly
all.equal(matrix_2000, sheet2000, check.attributes = FALSE)
# TRUE!
# But will that work for another year?

# Year 2001
matrix_2001 <- build_ncai_matrix(
  wb = wb,
  ityc = scot_2001_ityc,
  ed = ed,
  target_year = 2001,
  year_one = 2000)
# View(matrix_2001)
# It's matching in year 2.
all.equal(matrix_2001, sheet2001, check.attributes = FALSE)

# Year 2022
matrix_2022 <- build_ncai_matrix(
  wb = wb,
  ityc = scot_2022_ityc,
  ed = ed,
  target_year = 2022,
  year_one = 2000)
# View(matrix_2022)
# And then it's close but not perfect by 2022.
all.equal(matrix_2022, sheet2022, check.attributes = FALSE)

# These results show that after 22 years of the time series we have some
# drift. LLM thinks this is likely due to differences in how excel and R
# handle floating points.
# But we think this is too wrong to be that.

# 05-01-2026
# CL found transcription error in sheet 66 P55 - cell references previous
# year in indexing formula.
# Expect to find more errors like this, since we have now checked and the whole
# time series works except 2019 and 2022.
# This error is likely what breaks 2019.

# Can I check how big the
# differences are in a way that I understand better?
sheet2022 <- as.data.frame(sheet2022)
rownames(matrix_2022) <- rownames(sheet2022) <- habitat_codes

# Calculate Percentage Error
error_matrix <- (as.matrix(matrix_2022) / as.matrix(sheet2022) - 1) * 100

# Extract only the finite numbers (removes NaN and Inf)
clean_errors <- error_matrix[is.finite(error_matrix)]

# Summary of the drift (in percent)
summary(clean_errors)
# Worst case is out by 9% points.
# Can't say how bad that is without knowing the relative contribution of the
# thing - it could be forest adn really big or CVS and minimal.

# How many cells are exactly correct?
exact_matches <- sum(abs(clean_errors) < 0.00001, na.rm = TRUE)
total_cells <- length(clean_errors)
message(paste0("Exact matches: ", exact_matches, " out of ", total_cells,
               " (", round(exact_matches/total_cells*100, 2), "%)"))
# These will just be the ones where the data doesn't change!

# Make a heatmap
# install.packages("pheatmap")
library(pheatmap)

# Replace NaNs with 0 so the heatmap can render
plot_matrix <- error_matrix
plot_matrix[!is.finite(plot_matrix)] <- 0

pheatmap(plot_matrix,
         main = "Percentage Drift from Excel (2022)",
         display_numbers = FALSE,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         breaks = seq(-5, 5, length.out = 101)) # Focus on +/- 5% range

# Mean absolute error by habitat and service:
# Drift by Habitat (Rows)
habitat_drift <- rowMeans(abs(error_matrix), na.rm = TRUE)
habitat_summary <- data.frame(habitat = names(habitat_drift),
                              mae_percent = habitat_drift) %>%
  arrange(desc(mae_percent))

# Drift by Service (Columns)
service_drift <- colMeans(abs(error_matrix), na.rm = TRUE)
service_summary <- data.frame(service = names(service_drift),
                              mae_percent = service_drift) %>%
  arrange(desc(mae_percent))

head(habitat_summary, 10)
head(service_summary, 10)

# This is showing biggest problems to be:
# overestimation of the contrib of g1 broadleaved deiduous woodland
# underestimation of the contrib of g3 coniferous woodland
# Slight overestimation of e2 mesic grasslands (farming then, I think)
# These are areas with big extent but crucially they are 3/4 of areas where the
# extent data actually changes!
# Check market gardens which seems to be fine:
i1_drift <- error_matrix["i1", , drop = FALSE]

# 3. Summary of drift for i1
message("Summary of % drift for Habitat i1:")
print(summary(as.numeric(i1_drift)))

# 4. View specific services for i1 to see if some are worse than others
i1_drift_long <- as.data.frame(t(i1_drift)) %>%
  rename(percent_error = i1) %>%
  arrange(desc(abs(percent_error)))

head(i1_drift_long, 10)
# There is error - it's just small.

# Check for error trend by service type:
service_profile <- data.frame(
  service = all_service_labels,
  type = c(rep("Provisioning", length(provisioning_labels)),
           rep("Regulation & Maintenance", length(regulationandmaintenance_labels)),
           rep("Cultural", length(cultural_labels)))
)

error_summary_by_type <- as.data.frame(error_matrix) %>%
  pivot_longer(everything(), names_to = "service", values_to = "error_pct") %>%
  left_join(service_profile, by = "service") %>%
  filter(is.finite(error_pct)) %>% # Remove the NaNs (zero-valued cells)
  group_by(type) %>%
  summarise(
    mean_error = mean(error_pct),
    median_error = median(error_pct),
    max_error = max(error_pct),
    min_error = min(error_pct),
    sd_error = sd(error_pct),
    n_cells = n()
  )

print(error_summary_by_type)

library(ggplot2)

ggplot(as.data.frame(error_matrix) %>%
         pivot_longer(everything(), names_to = "service", values_to = "error_pct") %>%
         left_join(service_profile, by = "service") %>%
         filter(is.finite(error_pct)),
       aes(x = type, y = error_pct, fill = type)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Error Profile by Service Type (2022)",
       subtitle = "Percentage drift between R and Excel results",
       y = "Error (%)", x = "Service Section") +
  theme(legend.position = "none")
# Not much to see there.
# It seems to be more about the manually edited area figures which change
# annually.
# Perhaps natureScot will have something to say about that.

# Next step, calculate the NCAI indexed values, look at the whole time series,
# see how impactful this error is to the index.
