## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


#### SETUP ####
# install.packages("slider")

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)

#### Get existing index data ####
# Get Scotland data for replication from NatureScot spreadsheet:
# Existing bases and data (Scotland)

# Index of sheets in the NatureScot spreadsheet:
ns_sheets_path <- file.path("dev", "ncai.xlsx")
ns_sheets_index <- excel_sheets(ns_sheets_path)
ns_sheets_index

# Wellbeing base
ns_wellbeing_base <- read_xlsx(ns_sheets_path,
                sheet = 7,
                range = "F4:AG34",
                col_names = FALSE,
                col_types = "numeric",
                trim_ws = TRUE,
                .name_repair = "minimal"
                ) %>%
  as.data.frame()

# Ecosystem service potential base
ns_espb <- read_xlsx(ns_sheets_path,
                  sheet = 6,
                  range = "F4:AG34",
                  col_names = FALSE,
                  col_types = "numeric",
                  trim_ws = TRUE,
                  .name_repair = "minimal") %>%
  as.data.frame()

# Ecosystem service providing potential per SPU matrix:
ns_esppu <- read_xlsx(ns_sheets_path,
                  sheet = 3,
                  range = "F4:AG34",
                  col_names = FALSE,
                  col_types = "numeric",
                  trim_ws = TRUE,
                  .name_repair = "minimal") %>%
  as.data.frame()

# Habitat extent data years to 2022 (Scotland)
ns_habitat_extent <- readxl::read_excel(
    path = ns_sheets_path,
    sheet = 5,
    range = "E4:AA34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal" #quietens reporting on name repair
  ) %>%
  as.data.frame()

# Indicator directory
ns_indd <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 8,
  range = "N2:R106",
  col_names = TRUE,
  col_types = NULL,
  trim_ws = TRUE #,
  # .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames(c(st_labels,
          "comments",
          "used")) %>%
  select(-comments) %>%
  filter(used == "Yes")


# Importance scores ("Scotland weights")
# Between service-type scores:
ns_st_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D6:D8",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE #,
  # .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

ns_prov_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D13:D24",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE #,
  # .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

ns_regu_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D29:D39",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE #,
  # .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

ns_cult_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D44:D48",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE #,
  # .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

# Condition indicator scores, by year:
# Function read_the_ci_scores() gets the CI scores from NS sheets:
read_the_ci_scores <- function(sheet_path, # path to the spreadsheet
                               sheet_list, # list of sheets containing CI scores
                               vector_range # SINGLE-COLUMN range where scores
                               # are; must be same in each sheet.
) {

  # Initialise list of score vectors
  list_of_vectors <-  list()

  # Loop through list of sheets, reading in vector of scores
  for (idx in seq_along(sheet_list)) {

    actual_sheet_index <- sheet_list[idx]

    raw_score_data <- readxl::read_excel(
      path = sheet_path,
      sheet = actual_sheet_index,
      range = vector_range,
      col_names = FALSE,
      col_types = "numeric",
      trim_ws = TRUE,
      .name_repair = "minimal" #quietens reporting on name repair
    )

    vec <- as.numeric(raw_score_data[[1]])
    list_of_vectors[[paste0("ind", idx)]] <- vec

    # Confirmation message hopefully:
    cat("Processed column", idx, "(Sheet", actual_sheet_index, ")\n")
  }

  # Make list of vecs into df:
  ci_scores_df <- as.data.frame(list_of_vectors)

  return(ci_scores_df)

}

ns_ci_score_matrix <- read_the_ci_scores(sheet_path = ns_sheets_path,
                                         sheet_list = 10:47,
                                         vector_range = "I36:I58")
# need to add year column?



# LABELLING THE DATA
# Define labels
# Service-type labels:
st_labels <- c("provisioning", "regulation_and_maintenance", "cultural")
# Short version (needed?)
short_main_labels <- c("prov", "regu", "cult")
# Sets of ecosystem service labels by type:
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

# Habitat-type codes (required?)
habitat_codes <- c("b1", "b2", "b3", "c", "d1", "d2", "d4", "d5",
                "e1", "e2", "e4","e5", "e7",
                "f2","f3","f4","f9",
                "g1","g3","g4","g5","g6",
                "h2", "h3","i1","i2",
                "j1","j2","j3","j4","k")

# The range of years in the Scotland 23 extent data to be processed:
ns_year_list <- as.character(2000:2022)

# Apply labels
colnames(ns_espb) <- colnames(ns_esppu) <- colnames(ns_wellbeing_base) <-
  all_service_labels <-
  c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)

rownames(ns_espb) <- rownames(ns_esppu) <- rownames(ns_wellbeing_base) <-
  rownames(ns_habitat_extent) <- habitat_codes

colnames(ns_habitat_extent) <- ns_year_list

rownames(ns_ci_score_matrix) <- ns_year_list

####



#### CALCULATING NCAI ####

# RECREATING THE ES POTENTIAL BASE

# In the NatureScot spreadhsheet, ESPPU contains scores out of 5 indicating the
# likely of a service-providing unit to deliver its potential. So we will
# divide everything in that sheet by the max score 5 to get an ESPPU weight...

# EXCEPT in the case of the cells (habitat/ecosystem service combinations) in
# the ES Potential Base sheet: these have received a custom adjustment., viz
# they are divided by 0.2 (equiv. multiplied by 5). This can be applied at
# the point of dividing by max score: instead of 5, scores for these cells
# should be divided by 1.
#
# We build a matrix which records the custom divisor for each habitat/service
# type combination.

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

# Function make_custom_divisor_matrix() takes these and outputs a matrix of
# custom weights:
make_custom_divisor_matrix <- function(
    habitat_codes,
    all_service_labels,
    # long-form paired lists of habitat and service types to adjust:
    habitats_to_adjust,
    services_to_adjust,
    usual_divisor,
    custom_divisor
) {

  # Make a grid with all combinations of habitat and service.
  htst1 <- expand.grid(habitat = habitat_codes,
                       service_type = all_service_labels,
                       stringsAsFactors = FALSE)

  # Make a df which records all the cells to be adjusted:
  # htst2 is similar htst1, but only contains the combinations where we want a
  # different divisor:
  htst2 <- data.frame(
    habitat = habitats_to_adjust,
    service_type = services_to_adjust,
    divisor = custom_divisor,
    stringsAsFactors = FALSE
  )

  # Merge in the custom divisors, filling NAs with the usual divisor
  # We use left_join to keep everything in htst1 and bring in htst2
  htst <- htst1 %>%
    left_join(htst2, by = c("habitat", "service_type")) %>%
    mutate(divisor = replace_na(divisor, usual_divisor))

  # Pivot wider to get the same dimension df as esppu matrix.
  htst_wide <- htst %>%
    pivot_wider(names_from = service_type,
                values_from = divisor)

  return(htst_wide %>% select(-habitat))

}

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
# and converts it to weights by dividing by a common denominator, or a matrix
# in shape habitat/ecosystem service of custom divisors.
esppu_scores_to_weights <- function(
    esppu, # dataframe habitat type / ecosystem service
    divisor = NULL, # divisor for calculating weights from scores
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



## FUNCTION calc_espb() calculates the ecosystem service potential base. It
# takes the habitat extent data, year list, and ESPPU weights, and multiplies
# each habitat/service combination by the year one area of that habitat.
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
  # rownames(espb) <- habitat_labels # Unneccesary?

  return(espb)
}

# For Scotland:
scot_espb = calc_espb(habitat_extent = ns_habitat_extent,
                      esppu_weights = scot_esppu_weights,
                      year_list = ns_year_list
                      # ,habitat_labels = habitat_codes #unneccesary?
                      )

# Does the calculated scot_espb match the published ns_espb?
all.equal(ns_espb, scot_espb)
# Yes.



## RECREATING THE WELLBEING BASE
## The wellbeing base is recreated using the between- and within-service-type
# importance weights (the "Scotland weights").

# # FUNCTION imp_rtw_between()
# Gets between-service-provision-type IMPORTANCE weights from a dataframe of
# raw importance scores.
# Output is used in importance_rtw_within().
importance_rtw_between <- function(between_scores) {
  # between_scores is a vector of between-service-type importance scores
  between_weights <- between_scores / sum(between_scores) * 100

  return(between_weights)
}


## FUNCTION importance_rtw_within()
# Gets within-service-type importance weights from a dataframe of raw importance
# scores, using between weights output from importance_rtw_between().
# Used in calc_importance_weights() below.
importance_rtw_within <- function(within_scores, between_weights, index) {

  within_weights  <- within_scores / sum(within_scores) * between_weights[index, 1]

  return(within_weights)
}


## FUNCTION calc_importance_weights()
# Calculates importance weights, using within- and between-service-type weights.
# Loops through the list of ecosystem service types, calculating importance
# weights and returning a list of weight subset objects.

# Requires the vector of between-service-type scores, and a list of the
# within-service-type-score objects.
# Returns a list of numeric vectors of service-type subsets of importance
# weights.
calc_importance_weights <- function (between_scores, within_scores_list) {

  # Calculate the between weights
  b_weights <- importance_rtw_between(between_scores)

  # Map over the list and the indices simultaneously
  ww_subset_list <- lapply(seq_along(within_scores_list), function(i) {

    importance_rtw_within(
      within_scores   = within_scores_list[[i]],
      between_weights = b_weights,
      index           = i
    )
  })

  # Restore the names (prov, regu, cult) to the new list
  names(ww_subset_list) <- names(within_scores_list)

  return(ww_subset_list)
}

# eswr_scot_sections are the scottish between-ecosystem-service-type raw scores
# Make a list of the within-ecosystem_service_type scores
# Make a list of the within-service-type score sets:
ns_within_scores_list <- list(
  prov = ns_prov_importance_scores,
  regu = ns_regu_importance_scores,
  cult = ns_cult_importance_scores)

# Calculate Scotland's importance within-service-type weights:
scot_imp_weights_subsets <- calc_importance_weights(ns_st_importance_scores,
                                                    ns_within_scores_list)

## FUNCTION bind_imp_weights()
# Rejoins within-service-type weights back into one weight vector, applying
# between-service-type weights.

# Require list of importance within weight dataframes (output from
# imp_rtw_within() ) and list of all the service labels
bind_importance_weights <- function(within_weights_list,
                                    all_service_label_list) {

  # Safely get each subset of weights and flatten to one vector
  combined_weights <- unlist(lapply(within_weights_list, `[[`, 1), use.names = FALSE)

  # Check the list of weights is now the same length as list of all services
  if(length(combined_weights) != length(all_service_label_list)) {
    stop("Length mismatch: Total weights (", length(combined_weights),
         ") vs Labels (", length(all_service_label_list), ").")
  }

  # Put in wide format
  wide_joined_weights <- as.data.frame(t(combined_weights))
  colnames(wide_joined_weights) <- all_service_label_list

  return(wide_joined_weights)
}

# Rejoin the within-weight objects and pivot wide:
scot_importance_weights <- bind_importance_weights(
  within_weights_list = scot_imp_weights_subsets,
  all_service_label_list = all_service_labels)

# FIX MAYBE We could wrap these together.


## FUCNTION calc_wb() takes the ESPB (ES potential per habitat/service type
# combination) and expresses each cell as a proportion of the total potential
# for that service type across all habitats (colSums).
# Next, it multiplies in the importance weights (result of between and within
# service-type weighting process above).
# Returns the wellbeing base which is a matrix of habitat/service type.
calc_wellbeing_base <- function(espb, # ES potential, a matrix habitat/service type
                    importance_weights # Importance weights, a vector (wide df) by service type
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
  wellbeing_base <- sweep(
    x = espb_as_prop,
    MARGIN = 2,
    STATS = as.numeric(importance_weights),
    FUN = "*"
  )

  # Multiply by 100
  wellbeing_base <- wellbeing_base * 100

  return(wellbeing_base)

}

# Calculate Scotland's Well-being Base:
scot_wellbeing_base <- calc_wellbeing_base(espb = scot_espb,
                               importance_weights = scot_importance_weights)

# Is the calculated wellbeing base equal to NatureScot's wellbeing base?
all.equal(ns_wellbeing_base, scot_wellbeing_base)
# Yes.



#### PROCESS CONDITTION SCORES ####

# fns_bring_in_cirms() was used to harvest a binary CI relevance matrix in
# shape habitat/ecosystem service for each CI and save these as csv in the
# folder 'cirms'. They are regularly named to facilitate processing with
# functions below.

## CALCULATING THE WEIGHTED INDICATORS MATRICES
# For each indicator, the relevance matrix needs to be multiplied by the
# ecosystem-service-type weight for that indicator, as recorded in indd.

# To build the weighted relevance matrix we start by buiding a list of
# matrices of binary indicators of whether a CI is relevant for each combination
# of habitat/ecosystem service:
get_cirm_list <- function(spreadsheet_path, sheet_list, matrix_range) {

  # Loop through each sheet in the list
  list_of_dfs <- lapply(sheet_list, function(current_sheet) {

    # Read the data
    data <- readxl::read_excel(
      path = spreadsheet_path,
      sheet = current_sheet,
      range = matrix_range,
      col_names = FALSE,
      col_types = "numeric",
      trim_ws = TRUE,
      .name_repair = "minimal"
    ) %>%
      as.data.frame() %>%
      setNames(all_service_labels)

    data[] <- lapply(data, function(x) ifelse(!is.na(x) & x > 0, 1, 0))

    # Convert to dataframe
    return(as.data.frame(data))

  })

  # Return the full list of CIRMs
  return(list_of_dfs)
}

# Get list of relevance matrices for Scotland indicators
ns_cirms_list <- get_cirm_list(spreadsheet_path = ns_sheets_path,
                               sheet_list = 9:46,
                               matrix_range = "F4:AG34")




## FUNCTION build_ciwm
# This will take a CIRM object named cirm# where the # is the number of the CI.
# Also requires the list of service types and a list of label sets for each
# subtype.

build_ciwm_list <- function(cirm_list, st_list, label_subsets_list, indd) {

  # Use lapply to iterate over the indices of the cirm_list
  final_ciwm_list <- lapply(seq_along(cirm_list), function(ci_num) {

    # Extract the specific CIRM matrix for this iteration (e.g., ind1, ind2)
    cirm_object <- cirm_list[[ci_num]]

    # List to store weighted sub-matrices for this specific indicator
    ciwm_parts <- list()

    # Iterate through the actual names in st_list
    for (i in seq_along(st_list)) {

      # 1. Get the column labels for the CIRM subset
      current_labels <- label_subsets_list[[i]]

      # 2. Get the weight name from st_list (e.g., "provisioning_weight")
      weight_col_name <- st_list[[i]]

      # 3. Pull the numeric weight from indd
      # Row = current indicator (ci_num), Column = current weight type
      weight <- as.numeric(indd[ci_num, weight_col_name])

      # 4. Multiply subset of columns by weight
      sub_ciwm <- cirm_object[, current_labels, drop = FALSE] * weight

      # Store in our parts list
      ciwm_parts[[i]] <- sub_ciwm
    }

    # Combine parts side-by-side to recreate the full CIRM dimensions
    return(dplyr::bind_cols(ciwm_parts))
  })
  # Maintain list names
  names(final_ciwm_list) <- names(cirm_list)

  return(final_ciwm_list)
}

# Need list of service-type subsets of ecosystem service labels:
scot_label_subsets_list <- list(provisioning_labels,
                                regulationandmaintenance_labels,
                                cultural_labels)

# Build list of Condition Indicator Weighted Relevance matrices:
all_ciwms_list <- build_ciwm_list(cirm_list = ns_cirms_list,
                                  st_list = st_labels,
                                  label_subsets_list = scot_label_subsets_list,
                                  indd = ns_indd)


# E.g.
View(all_ciwms_list[[3]])
# Should look like first table in sheet '6' and it does.


# From the list of CIWMs, we should be able to recreate the Total Indicator
# Relevances matrix. That's the sum of relevance weights across indicators,
# plus the addon, found in sheet 'Total Indicator Relevances'.
# Call it TIR.
# A constant of 2 is added to the total indicator relevances in the NatureScot
# calculations. This may be to avoid zero divisions.

## FUNCTION calc_tir()
calc_tir <- function(all_ciwms_list, tir_constant) {

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + tir_constant

  return(tir)
}

# With the 2:
scot_tir_with2 <- calc_tir(all_ciwms_list = all_ciwms_list,
                           tir_constant = 2)
View(scot_tir_with2)


# Compare this calculated matrix with the TIR from the original sheet:
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

# Are they same?
all.equal(as.data.frame(ncai_tir_auto),
          scot_tir_with2,
          check.attributes = FALSE)
# Yes.



######## worked up to here on sat 10 jan

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

  col_idx <- ci_num

  # Access data directly by row name (year) and column index
  # We use [[ ]] to ensure we get a numeric value back, not a 1x1 dataframe
  raw_cond_score <- raw_cis[as.character(year_to_get), col_idx]
  year_one_score <- raw_cis[as.character(year_list[1]), col_idx]

  # Index calculation
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
