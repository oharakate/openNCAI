## DEVELOP CALCULATION PROCESS FOR openNCAI
# Kate O'Hara, Chris Littleboy
# 03-12-2025


# SETUP ####
library(dplyr)

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
rownames(espb) <- rownames(esppu) <-rownames(wb)  <-  spu_labels
# View(eds)
colnames(eds) <- eds_year_labels
# View(eds)

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
yeartocalc <- "2000"

# espu as weight - Looks like we need to take the value in the ESPPU sheet and
# treat it as a score out of 5. So divide everything in that by 5 to get a
# weight:
esppu_mat <- as.matrix(esppu)
esppu_aw  <- esppu_mat / 5
esppu_aw <- esppu_aw %>%
  as.data.frame()
# View(esppu_aw)

# Pull the vector for that year:
extyearvec <- ed %>%
  pull(yeartocalc)
made_espb <- sweep(
  x = esppu_aw,
  MARGIN = 1,
  STATS = extyearvec,
  FUN = "*"
)
# Does the calculated espb match the published one?
# No:
all.equal(espb, made_espb)
# And I'm not sure why.
# Many of the errors are small and could maybe be rounding but I doubt it.
View(made_espb)
View(espb)



#  We think...
# ESPB cell proportion of column
# gets multiplied by
# VECTOR ESPW Ecosystem service potential weighting
# to give
# WBB Wellbeing base
