## Function to create an object which facilitates custom adjustments to the
# esppu weights for Nature Scot
# Kate O'Hara
# 17-12-2025

# Background is that there are cells marked red in the ESPB sheet in which
# the weights are multiplied back up by 5 again.
# Here we will hard code the adjusted cells for now.

# Double check the up-to-date names of the lists of labels that are passed.

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

htst_wide <- adjust_esppu_weights(spu_codes, all_service_labels, cultural_labels)
