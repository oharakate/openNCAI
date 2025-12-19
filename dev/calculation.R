## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


# SETUP ####
# install.packages("slider")

library(dplyr)
library(tidyr)
library(tibble)
library(readr)

# Scotland data for replication:
# Existing bases (Scotland)
wb <- read.csv("dev/wellbeing_base.csv", header = FALSE)
espb <- read.csv("dev/ecosystem_potential_base.csv", header = FALSE)
# Ecosystem service providing potential per SPU matrix:
esppu <- read.csv("dev/ecosystem_potential_per_service_provisioning_unit.csv", header = FALSE)
# Extent data all years (Scotland)
eds_w_totals  <- read.csv("dev/ecosystem_area.csv", header = TRUE)
# R/m totals
eds <- eds_w_totals[-nrow(eds_w_totals),]
# Indicator directory
scot_indd <- read.csv("dev/scot_indicator_directory.csv", header = TRUE)
# Condition Indicator Relevance Matrix 1 (#2 Pollution orthophosphate etc.)
scot_cirm1 <- read.csv("dev/scot_cirm1.csv", header = FALSE)

# ES Potential ('Scotland weights')
# Chris has typed these manually below, but here they are as csv:
eswr_scot_sections <- read.csv("dev/scotland_weight_raw_service_sections.csv", header = FALSE)
eswr_prov <- read.csv("dev/scotland_weights_raw_provisioning.csv", header = FALSE)
eswr_regu <- read.csv("dev/scotland_weights_raw_regulationmaintenance.csv", header = FALSE)
eswr_cult <- read.csv("dev/scotland_weights_raw_cultural.csv", header = FALSE)

# Assign to more general terms:
st <- eswr_scot_sections

# Labels for data:
st_labels <- c("provisioning", "regulation_and_maintenance", "cultural")
short_main_labels <- c("prov", "regu", "cult")
main_weights <- c(10,20,10)
provisioning_weights <- c(20,15,9,9,20,13,13,7,11,12,1,0)
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
regulationandmaintenance_weights <- c(10,10,12,7,7,10,10,12,14,8,20)
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
cultural_weights <- c(20,20,20,20,20)
cultural_labels <- c("physical_experience",
                     "heritage_educational",
                     "aesthetic_entertainment",
                     "symbolic_sacred_religious",
                     "existence_bequest")

spu_codes <- c("b1", "b2", "b3", "c", "d1", "d2", "d4", "d5",
                "e1", "e2", "e4","e5", "e7",
                "f2","f3","f4","f9",
                "g1","g3","g4","g5","g6",
                "h2", "h3","i1","i2",
                "j1","j2","j3","j4","k")

# The range of years in the Scotland 23 extent data:
eds_year_labels <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007",
                "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")

# Apply labels
colnames(espb) <- colnames(esppu) <- colnames(wb) <- all_service_labels <-
  c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)

rownames(espb) <- rownames(esppu) <- rownames(wb) <- spu_labels

colnames(eds) <- eds_year_labels

####



#### CALCULATING NCAI ####

# RECREATING THE ES POTENTIAL BASE
# This section needs function-ised.
# Define extent data
ed <- eds
# Set a year (is this necessary?)
origin_year <- "2000"

# espu as weight - ESPPU contains scores out of 5 on the potential of a
# service-providing unit to deliver its potential. So divide everything in
# that by 5 to get a weight.


# EXCEPT in the case of the red cells, which are multiplied by 5.
# So we will make a matrix which records the divisor (will be 1 instead of 5
# for these adjusted ones).

## So let's make a NatureScot function, which will hard code in this
# adjustment in the first instance...
# This is in fns_ajust_esppu_weights.R

adjust_esppu_weights <- function(spu_codes,
                                 all_service_labels,
                                 cultural_labels) {

  # Make a grid with all combinations of habitat and service.
  htst1 <- expand.grid(spu = spu_codes,
                       service_potential = all_service_labels,
                       stringsAsFactors = FALSE)

  # Make a df which records all the cells to be adjusted:
  # htst2 is like htst1, but only contains the combinations where we want an
  # adjuster.
  # In this case, the adjuster is always 1, but it could vary in other
  # applications.
  # This section is hard coded and would need manually adjusted if any changes.
  htst2 <- data.frame(
    spu = c(rep("b1",7), rep("b2",5), rep("b3",5), "d1",
            rep("i2",6), rep("j1",5), rep("j2",5)),
    service_potential = c("erosion_mediation", "soil_formation_composition",
                          cultural_labels,
                          cultural_labels,
                          cultural_labels,
                          "climate",
                          "climate",
                          cultural_labels,
                          cultural_labels,
                          cultural_labels),
    constant = 1,
    stringsAsFactors = FALSE
  )

  # Merge in the custom adjusters, fill NAs with 5
  # We use left_join to keep everything in htst1 and bring in htst2
  htst <- htst1 %>%
    left_join(htst2, by = c("spu", "service_potential")) %>%
    mutate(constant = replace_na(constant, 5))

  # Pivot wider to get the same dimension df as esppu matrix.
  htst_wide <- htst %>%
    pivot_wider(names_from = service_potential,
                values_from = constant)

  return(htst_wide %>% select(-spu))

}


# Make the matrix of ScotNCAI adjustments to the weights:
htst_wide <- adjust_esppu_weights(spu_codes, all_service_labels, cultural_labels)

# And then use this in this function, which converts scores to weights,
# by default by dividing by 5, unless something like that ^ is passed to the
# custom_weight_matrix argument:

## FUNCTION esppu_scores_to_weights()
# Takes matrix of ESSPU scores and converts it to weights
esppu_scores_to_weights <- function(esppu, custom_weight_matrix = 5) {

  esppu_mat <- as.matrix(esppu)
  esppu_aw  <- (esppu_mat / as.matrix(custom_weight_matrix)) %>%
    as.data.frame()

  return(esppu_aw)
}

# For the Scottish data:
esppu_aw <- esppu_scores_to_weights(esppu, htst_wide)



# To calculate ESPB (ecosystem service potential base) for Scotland,
# we need Scotland's year one data.

# The years_from() function can collect the year range, if it is a consecutive
# list of years, but allow a custom list.

## FUNCTION years_from() takes a start year and number of years and returns a
# list of consecutive years.
years_from <- function(start_year, # a year, as integer numeric
                       end_year # the number of consecutive years to process
) {

  as.character(start_year:end_year)

}

# For Scotland:
scot_year_list <- years_from(2000, 2022)


## FUNCTION calc_espb() calculates the ecosystem service potential base. It
# takes ed the extent data, year_list and esppu_aw and multiplies each
# habitat/service combination by the year one area of that habitat.

calc_espb <- function(ed, esppu_aw, year_list, spu_labels) {

  year_one <- year_list[1]
  # Pull the vector for original year:
  originyearvec <- ed %>%
    pull(year_one)
  # These habitat extent values are multiplied by their esppu weightings:
  espb <- sweep(
    x = esppu_aw,
    MARGIN = 1,
    STATS = originyearvec,
    FUN = "*"
  )
  rownames(espb) <- as.character(spu_labels)

  return(espb)
}

# For Scotland:
scot_espb = calc_espb(ed = ed,
                      esppu_aw = esppu_aw,
                      year_list = scot_year_list,
                      spu_labels = spu_codes)

# View(espb)
# scot_espb is a matrix dims habitats * services.
# Does the calculated espb match the published one?
# Yes:
all.equal(espb, scot_espb, check.attributes = FALSE)
# ^ We are using check.attributes=FALSE to quiet messages about the types of
# labels and just see if the numbers/maths are correct.

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
imp_rtw_within <- function(scores, between_weights, index) {
  # Takes index 1-3 for the appropriate section.
  # Should improve this to make that list of indices soft maybe.
  # Maybe could add a section index column to the df holding the sets of
  # within weights?
  within_weights  <- scores / sum(scores) * between_weights[index, 1]

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
    ww_subset <- imp_rtw_within(scores = within_scores_list[[st_num]],
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

scot_wb <- calc_wb(scot_espb, scot_iw_within)

# Calculate Scotland's Well-being Base:
espb_totals <- colSums(espb)
espb_as_prop <- sweep(
  x = espb,
  MARGIN = 2,
  STATS = espb_totals,
  FUN = "/"
)
head(espb_as_prop)

scot_wb <- sweep(
  x = espb_as_prop,
  MARGIN = 2,
  STATS = as.numeric(scot_iw_within),
  FUN = "*"
)
scot_wb <- scot_wb * 100
# head(scot_wb)
# head(wb)
all.equal(wb, round(scot_wb, digits = 0), check.attributes = FALSE)
# TRUE!

# OK, so up to here we have been able to recreate the potential base and the
# wellbeing base from year 1 (2000) Scotland extent data and the two sheets
# which contain the ecosystem service potential per SPU (sheet 3) and the
# between and within service type 'importance to Scotland' weights (sheet 4).


#### How to get the Condition Indicators (CIs) ####
# OK, for each of the condition indicators...
# We will need a matrix of habitat * service, recording a binary value to
# denote for which combinations that indicator is relevant.
# Let's call these relevance matrices and number them cirm1

# We will split these into three matrices, one per ES type so.
# Instead of using prov, regu and cult, we should get numbers from a list of
# subtypes. (Should perhaps require a csv with service per row and a column for
# service type it belongs to, and draw numbered lists from that.)
# So...
# Let's get cirm1 (added code to read above)...

# REPLACED WITH FUNCTION BELOW
# head(scot_cirm1)
# the_cirm1 <- scot_cirm1
# names(the_cirm1) <- all_service_labels
# the_cirm1[is.na(the_cirm1)] <- 0
# Replaces any NA with 0.
#
# n_st <- nrow(st) # where st is the service types list
# for (i in 1:n_st) {
#
#    labels_name <- paste0("st", i, "_labels")
#    subrm_name  <- paste0("cirm1_", i)
#    wcol_name   <- paste0("st", i, "_weight")
#    subwm_name  <- paste0("ciwm1_", i)
#
#    assign(subrm_name, cirm1[ ,get(labels_name)], envir = .GlobalEnv)
#    weight <- indd[1, wcol_name]
#
#    assign(subwm_name, get(subrm_name) * weight, envir = .GlobalEnv)
#
# }
#
# head(the_cirm1)

# We will need a df which is a simplified version of the indicator directory
# sheet. Call it indd. It needs only rows with 'yes' for 'used'. It needs
# columns:
# ci_id
# st1_weight (service type 1 weight (provision for scotNCAI))
# st2_weight
# st3_weight
# And will keep the 'updated' field for now until we know if needed.
indd <- scot_indd # (was read in above)
head(indd)
# There is a weight column per service type and we will need to think about how
# we will index them. Perhaps we ask for the column numbers containing the
# weights? Hard code for now.
st1_labels <- provisioning_labels
st2_labels <- regulationandmaintenance_labels
st3_labels <- cultural_labels
# For each CI/ES service type relevance matrix (e.g. cirm1_prov), the cirm will
# be multiplied by the matching weight vector using sweep().
# E.g.

n_st <- nrow(st) # where st is the service types list
for (i in 1:n_st) {

  labels_name <- paste0("st", i, "_labels")
  subrm_name  <- paste0("cirm1_", i)
  wcol_name   <- paste0("st", i, "_weight")
  subwm_name  <- paste0("ciwm1_", i)

  assign(subrm_name, cirm1[ ,get(labels_name)], envir = .GlobalEnv)
  weight <- indd[1, wcol_name]

  assign(subwm_name, get(subrm_name) * weight, envir = .GlobalEnv)

}

ciwm_names_to_bind <- paste0("ciwm1_", 1:n_st)
ciwm_list <- mget(ciwm_names_to_bind)
ciwm1 <- Reduce(cbind, ciwm_list)

View(ciwm1)
# Check if that works to recreate table in sheet "2" of the scotNCAI spreadsheet.
scot_ciwm1 <- read.csv("dev/orig_scot_cirm1_weights.csv", header = FALSE)
scot_ciwm1[is.na(scot_ciwm1)] <- 0
names(scot_ciwm1) <- c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)
all.equal(scot_ciwm1, ciwm1)

## CHECKPOINT
# To this point we have replicated the indicator relevance weighted matrix for
# one of the numbered sheets ("2", pollution orthophosphate). We did this
# by indexing the correct cell in the directory of Condition Indicators /
# Ecosystem Service Type weights and multiplying that by a habitat/ES matrix
# indicating (binary) in which combinations that indicator is relevant.


# That has to happen for all the CIs.
# So let's work out how to loop through all CIs doing that...
# Will make a short version of the indicator directory
indd_short <- indd[1:3, ]

# This should become a function to read in a batch of CIRM csvs.
cirm1 <- read.csv("dev/scot_cirm1.csv", header = FALSE)
names(cirm1) <- all_service_labels
cirm1[is.na(cirm1)] <- 0
cirm1 <- cirm1 %>% mutate(across(everything(), as.numeric))

cirm2 <- read.csv("dev/scot_cirm2.csv", header = FALSE)
names(cirm2) <- all_service_labels
cirm2[is.na(cirm2)] <- 0
cirm2 <- cirm2 %>% mutate(across(everything(), as.numeric))

cirm3 <- read.csv("dev/scot_cirm3.csv", header = FALSE)
names(cirm3) <- all_service_labels
cirm3[is.na(cirm3)] <- 0
cirm3 <- cirm3 %>% mutate(across(everything(), as.numeric))

st1_labels <- provisioning_labels
st2_labels <- regulationandmaintenance_labels
st3_labels <- cultural_labels

n_cis <- nrow(indd_short)

for (j in 1:n_cis) {
  # We will need the numbered name of the cirm:
  cirm_name <- paste0("cirm", j)
  # And the numbered name of the ciwm we will make:
  ciwm_name <- paste0("ciwm", j)

  n_st <- nrow(st) # where st is the service types list
  for (i in 1:n_st) {

    labels_name <- paste0("st", i, "_labels")
    subrm_name  <- paste0(cirm_name, "_", i)
    wcol_name   <- paste0("st", i, "_weight")
    subwm_name  <- paste0(ciwm_name, i)

    assign(subrm_name, get(cirm_name)[ ,get(labels_name)], envir = .GlobalEnv)
    weight <- indd_short[j, wcol_name] # make sure change back to indd

    assign(subwm_name, get(subrm_name) * weight, envir = .GlobalEnv)

  }
  ciwm_names_to_bind <- paste0(ciwm_name, 1:n_st)
  ciwm_list <- mget(ciwm_names_to_bind)
  assign(ciwm_name, Reduce(cbind, ciwm_list))
}

scot_ciwm2 <- read.csv("dev/scot_ciwm2.csv", header = FALSE)
scot_ciwm2[is.na(scot_ciwm2)] <- 0
names(scot_ciwm2) <- all_service_labels
scot_ciwm3 <- read.csv("dev/scot_ciwm3.csv", header = FALSE)
scot_ciwm3[is.na(scot_ciwm3)] <- 0
names(scot_ciwm3) <- all_service_labels

all.equal(ciwm1, scot_ciwm1)
all.equal(ciwm2, scot_ciwm2)
all.equal(ciwm3, scot_ciwm3)

# CHECKPOINT
# So now we have reproduced a short set of the first 3 Condition Indicator
# Relevance Weight sets, by building from a matrix of binary is/not applicable
# entries, and the indicator directory which records the weight to be applied
# for each indicator, for each service type.

# And then I think these will be layered, added element-wise, on top of one
# another, to recreate the sheet 'Total Indicator Relevances'.
# We will use 'element-wise summation'

# Create a character vector of all the CI weight matrix names:
n_cis <- nrow(indd_short)
ciwm_names <- paste0("ciwm", 1:n_cis)
# Get a list of the actual objects
ciwm_list <- mget(ciwm_names)
# Use Reduce() to add all df in the list element wise.
ciwm_total <- Reduce("+", ciwm_list)
View(ciwm_total)

# CHECKPOINT
# Now we can replicate in ciwm_total the Total Indicator Relevances sheet.
# NOTE THAT for now we've just done this with SHORT SET OF THREE indicator
# relevance matrices and the weights from the indicator directory.
# Next we will need actual condition indicator scores.
# Once this is all working we can get the full set for all scot CIs of:
# - CI relevance matrix (binary)
# - Yearly condition score vector
# - And possibly the bits to make that vector - see next comment...


##### not doing the smoothing INDIVIDUAL CONDITION INDICATOR INDEXING ####
# In general this is how it works:
# 1. We have a value of some indicator for each year.
# 2. We calculate a smoothed value for year x like this:
#    xsmoothed = (x + (x-1) + (x-2) + (x-3) + (x-4)) / 5
# 3. We index the value against the initial year's value:
#    yearx index = xsmoothed / year1x * 100
#    Note year 1 is indexed as 100.
# 4. Something we need to watch out for is that score to be indexed (STBI)
#    comes in in different ways. E.g. sheet "4" uses the sum of two scores.
#    They are added to give the STBI. Do we want to recreate that for Scotland?
#    If other countries use openNCAI, the work of calculating the STBI is
#    probably for them to do.
#    For now I will code with the STBI. And maybe we write
#    custom functions for the Scottish index ultimately? Or, and we can think
#    about publishing support resources here, we provide an Excel workbook with
#    guidance in it from which users can easily extract csvs after doing their
#    own calculations. I feel we will need to provide guidance for things like
#    the esppu and esp within-between between weights, and the indicator
#    directory.

# For the first 3 Scottish CIs I am taking the vector of scores, before
# smoothing and before indexing. This often means filling forwards or backwards.
# E.g. in Sheet 2, CI1, there is data to 2018, so the 2018 value is pasted in
# for the following years up to 2022. The finished vector should be 23 long in
# each case.


## OK, GOING TO JUST WORK WITH THE VECTOR OF NON-INDEXED SCORES
# Proceeding on the basis that any smoothing and inter/extrapolation is done
# by the user as the expert.
n_cis <- nrow(indd_short)

stbi1 <- read.csv("dev/scot_ci1_stbi.csv", header = FALSE)
stbi2 <- read.csv("dev/scot_ci2_stbi.csv", header = FALSE)
stbi3 <- read.csv("dev/scot_ci3_stbi.csv", header = FALSE)

# Get a list of these (names, then objects)
stbi_names <- paste0("stbi", 1:n_cis)
stbi_list <- mget(stbi_names)
# Put them into a matrix (can require a matrix in future but do this for now):
stbi <- data.frame(sapply(stbi_list, function(df) df[[1]])) %>%
  setNames(stbi_names)

# Now we have the whole Scottish matrix of raw scores:
scot_stbi <- read_csv(
  file.path("dev", "scot_year_ci_matrix.csv"),
  col_names = TRUE
  )

## FUNCTION index_scores() converts matrix of year/ci raw scores to year/ci
## indexed scores. Returns matrix of indices.
index_scores <- function(score_matrix, n_cis) {

  scorecol_names <- paste0("stbi", 1:n_cis)
  names(score_matrix) <- scorecol_names
  working_matrix <- score_matrix

  for (i in 1:n_cis) {
    score_name <- paste0("stbi", i)
    indic_name <- paste0("ind", i)

    y1score <- score_matrix %>%
      pull(!!score_name) %>%
      .[1]

    working_matrix <- working_matrix %>%
      mutate(!!indic_name := (!!sym(score_name) / y1score) * 100)
    # I'm not really familiar with the !! := and !!sym() operators.
    # LLM recommendation!
  }
  index_matrix <- working_matrix %>%
    select(-all_of(scorecol_names))

  return(index_matrix)
}

# Convert matrix of Scottish year / raw CI scores to indexed values:
# Number of CIs is the rows in the indicator directory.
scot_cis <- index_scores(scot_stbi, nrow(scot_indd))
head(scot_cis)
# Remember it's important that the unrounded values of raw scores went in.


## FUNCTION ciwm_to_cirm will load a bunch of regularly named CIRMs
# and replace all non-zero and non-missing values with 1, and replace all NAs
# with 0.
# Don't need this here because it's done in bring_in_cirms.R.

# Used bring_in_cirms.R to batch process the cirm matrices
# from the NCAI sheet.

# Here is a function to bring all the cirms into the environment:
# But this tends to break R.

n_cis <- nrow(indd)
n_cis
for (i in n_cis) {
  object_name <- paste0("cirm", i)
  csv_to_read <- file.path("dev", paste0("scot_cirm", i, ".csv"))
  df <- read.csv(csv_to_read, header = TRUE)
  assign(object_name, df, envir = .GlobalEnv)
}


# So now we can loop through all 38 CIs, multiplying binary relevance matrix by
# weights from indd, and then adding all together.


# FUNCTIONISE THIS:
# This should become a function to read in a batch of CIRM csvs.


st1_labels <- provisioning_labels
st2_labels <- regulationandmaintenance_labels
st3_labels <- cultural_labels

n_cis <- nrow(indd_short)



build_ciwm <- function(ci_num) {
  # ci_num is the number of the CI to work with
  # CIRM objects named cirm1, cirm2, etc. are expected.

  # Build the numbered name of the cirm object
  cirm_name <- paste0("cirm", ci_num)
  # Build the numbered name of the ciwm to be built
  ciwm_name <- paste0("ciwm", ci_num)

  n_st <- nrow(st) # where st is the list of service

  # Initialise list of temp object to clean up later
  temp_objects_to_remove <- c()

  # Iterate through service types, applying weights to CIRMs.
  for (i in 1:n_st) {
    labels_name <- paste0("st", i, "_labels")
    subrm_name  <- paste0(cirm_name, "_", i)
    wcol_name   <- paste0("st", i, "_weight")
    subwm_name  <- paste0(ciwm_name, "_", i)

    # Construct list of temp objects to remove.
    temp_objects_to_remove <- c(temp_objects_to_remove, subrm_name, subwm_name)

    # select subset of the CIRM
    assign(subrm_name, get(cirm_name)[ ,get(labels_name)], envir = .GlobalEnv)
    # select correct weight
    weight <- indd[ci_num, wcol_name]
    # create subset of weight matrix by multiplying together <-
    assign(subwm_name, get(subrm_name) * weight, envir = .GlobalEnv)

  }

  # Join the service type subsets back together:
  ciwm_names_to_bind <- paste0(ciwm_name, "_", 1:n_st)
  ciwm_list <- mget(ciwm_names_to_bind, envir = .GlobalEnv)
  assign(ciwm_name, Reduce(cbind, ciwm_list), envir = .GlobalEnv)

  # Remove temp objects
  rm(list = temp_objects_to_remove, envir = .GlobalEnv)
}

# This would loop through, creating the ciwm object in the environment.
# However, this is going to create an awful lot of objects.
# Work towards including the layering step and then rm-ing all the extraneous
# things. That could iteratively add the layers to the matrix.
n_cis <- nrow(indd)
for (ci_num in 1:n_cis) {
  build_ciwm(ci_num)
}








# CHECKPOINT


# And then I think these will be layered, added element-wise, on top of one
# another, to recreate the sheet 'Total Indicator Relevances'.
# We will use 'element-wise summation'

# Create a character vector of all the CI weight matrix names:
n_cis <- nrow(indd_short)
ciwm_names <- paste0("ciwm", 1:n_cis)
# Get a list of the actual objects
ciwm_list <- mget(ciwm_names)
# Use Reduce() to add all df in the list element wise.
ciwm_total <- Reduce("+", ciwm_list)
View(ciwm_total)
