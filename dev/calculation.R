## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


# SETUP ####
library(dplyr)
library(tidyr)
library(tibble)

# Scotland data for replication:
# Existing bases (Scotland)
wb   <- read.csv("dev/wellbeing_base.csv", header = FALSE)
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

spu_labels <- c("b1", "b2", "b3", "c", "d1", "d2", "d4", "d5",
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

rownames(espb) <- rownames(esppu) <- rownames(wb)
rownames(esw_scot_prov) <- provisioning_labels
rownames(esw_scot_regu) <- regulationandmaintenance_labels
rownames(esw_scot_cult) <- cultural_labels

colnames(eds) <- eds_year_labels

####



#### CALCULATING NCAI ####
# Define extent data
ed <- eds
# Set a year:
origin_year <- "2000"

# espu as weight - ESPPU contains scores out of 5 on the potential of a
# service-providing unit to deliver its potential. So divide everything in
# that by 5 to get a weight.
# EXCEPT in the case of the red cells, which are multiplied by 5.
# So we will make a matrix which records the divisor (will be 1 instead of 5
# for these adjusted ones).

# From Chris:
# Make a grid with all combinations of habitat and service.
t1 <- expand.grid(spu_labels, all_service_labels)
head(t1)
# Make a df which records all the cells to be adjusted:
# (Potentially, we would in time produce a workbook with sheets to be filled
# in by new users of openNCAI. We could
# provide labelled twide below. )
# t2 is like t1, but only contains the combinations where we want an ajuster.
# In this case, the adjuster is always 1, but it could vary in other
# applications.
t2 <- data.frame(spu = c(rep("b1",7),rep("b2",5), rep("b3",5),
                         "d1",
                         rep("i2",6),
                         rep("j1",5),
                         rep("j2",5)),
                 service_potential = c("erosion_mediation", "soil_formation_composition",
                                       cultural_labels,
                                       cultural_labels,
                                       cultural_labels,
                                       "climate",
                                       "climate",
                                       cultural_labels,
                                       cultural_labels,
                                       cultural_labels),
                 constant = 1)
# t2
# Label combinations df cols
colnames(t1) <- c("spu", "service_potential")
# Merge in the to-be-adjusted. Non-matches will get NA.
t <- merge(t1,t2, all = TRUE)
# Replace NA with 5 as this is the correct divisor.
# Adjustees get 1., the constant we specified.
t$constant[is.na(t$constant)] <- 5
# Pivot wider to get the same dimension df as esppu matrix.
twide <- pivot_wider(t, id_cols = spu,
                     names_from = service_potential,
                     values_from = constant)[,2:29]

# Divide esppu potential/unit scores by 5, unless adjustment specified, by
# dividing esppu by twide, to get pottential per unit weights.
# Now onto building espb from esppu & year 1 habitat extent data:
# Make into matrix
esppu_mat <- as.matrix(esppu)
# Divide by the same sized matrix we made above, holding the divisors.
esppu_aw  <- esppu_mat / twide
# Back to df.
esppu_aw <- esppu_aw %>%
  as.data.frame()
# View(esppu_aw)


# To get an ecosystem service potential base for Scotland, we need Scotland's
# year one data.
# Pull the vector for original year:
originyearvec <- ed %>%
  pull(origin_year)
# These habitat extent values are multiplied by their esppu weightings:
scot_espb <- sweep(
  x = esppu_aw,
  MARGIN = 1,
  STATS = originyearvec,
  FUN = "*"
)
rownames(scot_espb) <- spu_labels
# scot_espb is a matrix dims habitats * services.
# Does the calculated espb match the published one?
# Yes:
all.equal(espb, scot_espb)


## Next step is to recreate the wellbeing base.

# # FUNCTION imp_rtw_between()
# Gets between-service-provision-type IMPORTANCE weights from a df of raw
# scores:

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
imp_rtw_within <- function(scores, between_weights, index) {
  # Takes index 1-3 for the appropriate section.
  # Should improve this to make that list of indices soft maybe.
  # Maybe could add a section index column to the df holding the sets of
  # within weights?
  within_weights  <- scores / sum(scores) * between_weights[index,]

  return(within_weights)
}

# Calculate Scotland's importance weights:
scot_between_scores <- imp_rtw_between(eswr_scot_sections)
iw_prov <- imp_rtw_within(eswr_prov, scot_between_scores, 1)
iw_regu <- imp_rtw_within(eswr_regu, scot_between_scores, 2)
iw_cult <- imp_rtw_within(eswr_cult, scot_between_scores, 3)

# Join these into one vector
scot_iw_within <- rbind(iw_prov, iw_regu, iw_cult)
rownames(scot_iw_within) <- (c(provisioning_labels,
                   regulationandmaintenance_labels,
                   cultural_labels))
colnames(scot_iw_within) <- ("weight")
scot_iw_within <- scot_iw_within %>%
  rownames_to_column(var = "service") %>%
  pivot_wider(
    names_from = service,
    values_from = weight
  )


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
all.equal(wb, round(scot_wb, digits = 0))
# TRUE!

# OK, so up to here we have been able to recreate the potential base and the
# wellbeing base from year 1 (2000) Scotland extent data and the two sheets
# which contain the ecosystem service potential per SPU (sheet 3) and the
# between and within service type 'importance to Scotland' weights (sheet 4).


#### How to get the Condition Indicators (CIs) ####
# OK, for each of the condition indicators...
# And I was expecting 38 but it seems like there are 39 entries marked 'yes'
# for 'used' in the indicator directory sheet...
# We will need a matrix of habitat * service, recording a binary value to
# denote for which combinations that indicator is relevant.
# Let's call these relevance matrices and number them cirm1

# We will split these into three matrices, one per ES type so.
# Instead of using prov, regu and cult, we should get numbers from a list of
# subtypes. (Should perhaps require a csv with service per row and a column for
# service type it belongs to, and draw numbered lists from that.)
# So...
# cirm1_1 (type 1 (prov) relevance matrix of service type 1)
# cirm1_2 (type 1 (prov) relevance matrix of service type 2)
# cirm1_3 (type 1 (prov) relevance matrix of service type 3)
# Let's get cirm1 (added code to read above)...
head(scot_cirm1)
cirm1_labels <- c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)
cirm1 <- scot_cirm1
names(cirm1) <- cirm1_labels
cirm1[is.na(cirm1)] <- 0
  # Replaces any NA with 0.
head(cirm1)

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

# Check if that works to recreate table insheet "2" of the scotNCAI spreadsheet.
scot_ciwm1 <- read.csv("dev/orig_scot_cirm1_weights.csv", header = FALSE)
scot_ciwm1[is.na(scot_ciwm1)] <- 0
names(scot_ciwm1) <- c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)
all.equal(scot_ciwm1, ciwm1)

## CHECKPOINT
# To this point we have replicated the indicator relevance weight matrix for
# one of the numbered sheets ("2", pollution orthophosphate). We did this
# by indexing the correct cell in the directory of Condition Indicators /
# Ecosystem Service Type weights and multiplying that by a habitat/ES matrix
# indicating (binary) in which combinations that indicator is relevant.


# That has to happen for all the CIs.
# So let's work out how to loop through all CIs doing that...
n_cis <- length(indd)

for (i in 1:n_cis) {
  print(1)
}




# And then I think these will be layered, added element-wise, on top of one
# another.
n_cis <- length(ci_dir)

ciwm <- ciwm1
for (i in 1:n_cis) {
  current_ciwm_name <- paste0("ciwm", i)
  current_ciwm <- get(current_ciwm_name, 1)

  ciwm <- ciwm + current_ciwm
}

# The matrices need to be put back together into one big one


# Assume here we will use a loop or list apply to go through them all.
# How to get the number from the name?
# my_list <- list()
# for (i in 1:10) {
#   my_list[[i]] <- get(paste0("cirm", i))
# }
# And use assign() to assign data to an object and use that number in the name
