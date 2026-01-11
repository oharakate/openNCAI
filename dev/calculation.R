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

#### DEFINE LABELS AND LISTS FOR NS DATA ####

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

# All service labels in order:
all_service_labels <- c(provisioning_labels,
                        regulationandmaintenance_labels,
                        cultural_labels)

# Habitat-type codes (required?)
habitat_codes <- c("b1", "b2", "b3", "c", "d1", "d2", "d4", "d5",
                   "e1", "e2", "e4","e5", "e7",
                   "f2","f3","f4","f9",
                   "g1","g3","g4","g5","g6",
                   "h2", "h3","i1","i2",
                   "j1","j2","j3","j4","k")

# The range of years in the Scotland 23 extent data to be processed:
ns_year_list <- as.character(2000:2022)


#### IMPORT EXISTING DATA FROM NS SHEETS ####
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
  trim_ws = TRUE,
  .name_repair = "minimal" # quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

# FIX these three should become a list maybe?
ns_prov_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D13:D24",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

ns_regu_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D29:D39",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>%
  setNames("score")

ns_cult_importance_scores <- readxl::read_excel(
  path = ns_sheets_path,
  sheet = 4,
  range = "D44:D48",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
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

# Import the Condition Indicators from NS sheet:
ns_ci_score_matrix <- read_the_ci_scores(sheet_path = ns_sheets_path,
                                         sheet_list = 9:46,
                                         vector_range = "I36:I58")



# Function read_ns_year_sheet() gets the main matrix of contributions (based on
# habitat condition scores and relevance weighting of condition indicators)
# from one of the year named sheets (e.g. "2000"):
read_ns_year_sheet <- function(sheet, path, labels) {

  year_sheet <- readxl::read_excel(
    path = path,
    sheet = sheet,
    range = "F4:AG34",
    col_names = FALSE,
    col_types = "numeric",
    trim_ws = TRUE,
    .name_repair = "minimal" #quietens reporting on name repair
  ) %>%
    as.data.frame() %>% #make sure DF
    setNames(labels) #give same column names

  # NAs to 0 as before
  year_sheet[is.na(year_sheet)] <- 0

  return(year_sheet)

}

# Get a list of the matrices in all the NS year sheets:
ns_year_sheets_ids <- 50:72
ns_all_year_sheets <- lapply(X = ns_year_sheets_ids,
                             FUN = read_ns_year_sheet,
                             path = ns_sheets_path,
                             labels = all_service_labels)

# e.g. this should look like the year 2000:
View(ns_all_year_sheets[[1]])



#### APPLY LABELS TO IMPORTED DATA ####


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

#### RECREATING THE ES POTENTIAL BASE ####

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
ns_all_ciwms_list <- build_ciwm_list(cirm_list = ns_cirms_list,
                                  st_list = st_labels,
                                  label_subsets_list = scot_label_subsets_list,
                                  indd = ns_indd)


# E.g.
# View(ns_all_ciwms_list[[3]])
# Should look like first table in sheet '6' and it does.


# From the list of CIWMs, we should be able to recreate the Total Indicator
# Relevances matrix. That's the sum of relevance weights across indicators,
# plus the addon, found in sheet 'Total Indicator Relevances'.
# Call it TIR.
# A constant of 2 is added to the total indicator relevances in the NatureScot
# calculations. This may be to avoid zero divisions. The constant multiplied
# by 100 will be included when we sum the relevance-weighted contributions of
# quality indicators in the following step (such that indexed on itself it
# always equals 1).

## FUNCTION calc_tir()
calc_tir <- function(all_ciwms_list, tir_constant) {

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + tir_constant

  return(tir)
}

# For Scotland:
ns_tir_constant = 2
scot_tir <- calc_tir(all_ciwms_list = ns_all_ciwms_list,
                     tir_constant = ns_tir_constant)
# Should look like NS sheet "Total Indicator Relevances":
# View(scot_tir)
# It does.


# Compare this calculated matrix with the TIR from the original sheet:
ns_sheets_index
# Use sheet 75:
ns_tir <- readxl::read_excel(
  path = "dev/ncai.xlsx",
  sheet = 74,
  range = "F4:AG34",
  col_names = FALSE,
  col_types = "numeric",
  trim_ws = TRUE,
  .name_repair = "minimal" #quietens reporting on name repair
) %>%
  as.data.frame() %>% #make sure DF
  setNames(all_service_labels) #give same column names

# Are they same?
all.equal(ns_tir, scot_tir)
# Yes.



#### Generate YWCCMS ####

# For any year, yearly weighted condition contribution matrix YWCCM for each
# indicator can be generated by multiplying the CIWM (condition indicator
# weights matrix) by the year's raw condition scores ICC (around 100).

## FUNCTION get_yearly_condition() extracts the indexed condition score for one
# CI in one year:
get_yearly_condition <- function(raw_cis, year_to_get, ci_num, year_list) {

  col_idx <- ci_num

  # Access data directly by row name (year) and column index
  raw_cond_score <- raw_cis[as.character(year_to_get), col_idx]
  year_one_score <- raw_cis[as.character(year_list[1]), col_idx]

  # Index calculation
  indexed_cond_score <- (raw_cond_score / year_one_score) * 100

  return(as.numeric(indexed_cond_score))
}



# FUNCTION build_all_ywccms() multiplies one year's indexed value by the
# relevance weight matrix (CIWM) for all CIs in the list. Takes all_ciwms_list
# and outputs all_ywccms_list. These objects are not explicitly in the NS
# spreadsheet; rather they are equvalent to the the first table in the numbered
# indicator sheets multiplied by the year value in P36 thru 58
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



# Next we will sum together the YWCCMs for all indicators to create the Total
# Yearly Contribution of Scotland's habitats to ecosystem services in each year.
# FUNCTION build_tyc() takes the list of indicator ywccms for a year, adds in
# the constant numerator term * index of 100 and divides by the total relevances
# (including constant term again):
build_tyc <- function(list_of_ywccms, tir, tir_constant) {

  # Get sum of yearly weighted condition contributions for a year
  sum_ywccms <- Reduce("+", list_of_ywccms)

  # Add the TIR constant * 100 and divide by TIR
  tyc <- (sum_ywccms + (100 * tir_constant)) / tir

  return(tyc)
}



# FUNCTION build_all_tycs(), for each year in the series, uses build_all_yccms()
# to generate the yearly relevance-weighted contribution for each CI, before
# using build_tyc() to combine these. Outputs a list of TYCs per year (can be
# understood as the yearly flow of ecosystem service per habitat, and will be
# indexed upon its year one values and multiplied by the habitat extent and
# the wellbeing base to gain the raw measure of natural capital assets.)
build_all_tycs <- function(raw_cis, year_list, ciwms_list, tir, tir_constant) {

  # Call the process for every year in the list
  raw_tyc_list <- lapply(year_list, function(yr) {

    # STEP A: Build all the individual indicator matrices for THIS year
    current_year_ywccms <- build_all_ywccms(
      raw_cis = raw_cis,
      year = yr,
      year_list = year_list,
      ciwms_list = ciwms_list
    )

    # STEP B: Sum them and normalize using the updated build_tyc
    tyc <- build_tyc(
      list_of_ywccms = current_year_ywccms,
      tir = tir,
      tir_constant = tir_constant
    )

    return(tyc)
  })

  names(raw_tyc_list) <- year_list
  return(raw_tyc_list)
}




# For Scotland:
scot_tycs_list <- build_all_tycs(raw_cis = ns_ci_score_matrix,
                                year_list = ns_year_list,
                                ciwms_list = ns_all_ciwms_list,
                                tir = scot_tir,
                                tir_constant = ns_tir_constant)


# FUNCTION build_ncai_matrix builds a matrix for a year as per the year sheets:
build_ncai_matrix <- function(tyc, wellbeing_base, habitat_extent, target_year, year_one) {

  # Convert to characters for safe indexing
  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors
  extent_target_vec <- habitat_extent[[target_str]]
  extent_origin_vec <- habitat_extent[[origin_str]]

  # Index the habitat extent values
  extent_index <- (extent_target_vec / extent_origin_vec * 100)
  # extent_index[!is.finite(extent_index)] <- 0 # hopefully redundant?

  # Multiply the wellbeing base by the TYC
  # Ensure they are both treated as matrices for element-wise multiplication
  wb_tyc <- as.matrix(tyc) * as.matrix(wellbeing_base)

  # Apply the extent index across the rows (Habitats)
  # sweep() works perfectly here: MARGIN 1 applies extent_index[i] to every cell in row i
  ncai_matrix <- sweep(
    x = wb_tyc,
    MARGIN = 1,
    STATS = extent_index,
    FUN = "*"
  )

  # Divide by 10,000 as per your sheet calculation
  return(as.data.frame(ncai_matrix / 10000))
}

# FUNCTION build_all_nca_matrices() builds the year sheet for every year in the
# year list:
build_all_ncai_matrices <- function(tyc_list, wellbeing_base, habitat_extent, year_one) {

  # Iterate over the names (years) of the tyc_list
  all_ncai <- lapply(names(tyc_list), function(yr) {

    build_ncai_matrix(
      tyc = tyc_list[[yr]],
      wellbeing_base = wellbeing_base,
      habitat_extent = habitat_extent,
      target_year = yr,
      year_one = year_one
    )
  })

  names(all_ncai) <- names(tyc_list)
  return(all_ncai)
}

scot_ncai_list <- build_all_ncai_matrices(tyc_list = scot_tycs_list,
                                          wellbeing_base = ns_wellbeing_base,
                                          habitat_extent = ns_habitat_extent,
                                          year_one = ns_year_list[[1]]
)

View(scot_ncai_list[[1]])
View(scot_ncai_list[[23]])

# NOW THESE LOOK CORRECT.
# We can test the whole series:
comparison_results <- mapply(function(list1, list2) {
  all.equal(list1, list2)
}, scot_ncai_list[1:23], ns_all_year_sheets[1:23], SIMPLIFY = FALSE)

# We find errors in years 2019 and 2022:
comparison_results

# After close inspection we are able to pinpoint transcription errors in the
# spreadsheet.
