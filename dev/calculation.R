## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


# SETUP ####
library(dplyr)
library(tidyr)

# Data

wb   <- read.csv("dev/wellbeing_base.csv", header = FALSE)
espb <- read.csv("dev/ecosystem_potential_base.csv", header = FALSE)
esppu <- read.csv("dev/ecosystem_potential_per_service_provisioning_unit.csv", header = FALSE)
eds_w_totals  <- read.csv("dev/ecosystem_area.csv", header = TRUE)
eds <- eds_w_totals[-nrow(eds_w_totals),]

# Labels for data:

main_labels <- c("provisioning", "regulation_and_maintenance", "cultural")
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

colnames(espb) <- colnames(esppu) <- colnames(wb) <-
  c(provisioning_labels,regulationandmaintenance_labels,cultural_labels)

all_service_labels

rownames(espb) <- rownames(esppu) <-rownames(wb)  <-  spu_labels
# View(eds)
colnames(eds) <- eds_year_labels
# View(eds)

# Make a matrix of colnames and rownames:

snum_labels <- data.frame(c("1.1",
                "1.2",
                "1.3",
                "1.4",
                "1.5",
                "1.6",
                "1.7",
                "1.8",
                "1.9",
                "1.10",
                "1.11",
                "1.12",
                "2.1",
                "2.2",
                "2.3",
                "2.4",
                "2.5",
                "2.6",
                "2.7",
                "2.8",
                "2.9",
                "2.10",
                "2.11",
                "3.1",
                "3.2",
                "3.3",
                "3.4",
                "3.5"))

s_labels <- c(provisioning_labels, regulationandmaintenance_labels, cultural_labels)


column_labels <- c(provisioning_labels, regulationandmaintenance_labels, cultural_labels)
label_mat <- expand.grid(Row = spu_labels, Column = column_labels)
head(label_mat)

list_labels <- list(
  c(
    provisioning_labels,
    regulationandmaintenance_labels,
    cultural_labels
    )
  )
column_labels <-  do.call(c, list_labels)
print(length(column_labels))
label_mat <- expand.grid(Row = spu_labels, Column = column_labels)

head(label_mat)
tail(label_mat)

#### CALCULATING NCAI ####
# Things in use
# MATRIX EDS: VECTOR per year of EA Ecosystem Area
# which we think gets multiplied by
# ESPU Ecosystem service potential per unit/hectare
# to make
# MATRIX E

# Define extent data
ed <- eds
# Set a year:
origin_year <- "2000"

# espu as weight - Looks like we need to take the value in the ESPPU sheet and
# treat it as a score out of 5. So divide everything in that by 5 to get a
# weight
# EXCEPT in the case of the read cells, which are multiplied by 5.
# So we will make a matrix which records the divisor (will be 1 instead of 5
# for these adjusted ones).

# From Chris:
# Make a grid with all combinations of habitat and service.
t1 <- expand.grid(spu_labels, s_labels)
head(t1)
# Make a df which records all the cells to be adjusted:
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

# Now onto building espb from esppu & year 1 habitat extent data:
# Make into matrix
esppu_mat <- as.matrix(esppu)
# Divide by the same sized matrix we made above, holding the divisors.
esppu_aw  <- esppu_mat / twide
# Back to df.
esppu_aw <- esppu_aw %>%
  as.data.frame()
# View(esppu_aw)

# Pull the vector for original year:
originyearvec <- ed %>%
  pull(origin_year)
made_espb <- sweep(
  x = esppu_aw,
  MARGIN = 1,
  STATS = originyearvec,
  FUN = "*"
)
rownames(made_espb) <- spu_labels
# Does the calculated espb match the published one?
# Yes:
all.equal(espb, made_espb)


