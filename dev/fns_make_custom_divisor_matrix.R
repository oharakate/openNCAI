## Function to create an object which facilitates custom adjustments to the
# esppu weights for Nature Scot
# Kate O'Hara
# 17-12-2025

# Background is that there are cells marked red in the ESPB sheet in which
# the weights are multiplied back up by 5 again.
# We will make these adjustments by changing the amount by which the ESPPU
# score is divided to give the ESPPU weight (instead of 5, some are divided
# by 1).
# This function takes long-form paired lists to record in which habitat/service-
# type combinations the divisor should be adjusted, along with the custom
# divisor (expects all custom divisors have same value).

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
  # htst2 is like htst1, but only contains the combinations where we want an
  # adjuster.
  # In this case, the adjuster is always 1, but it could vary in other
  # applications.
  htst2 <- data.frame(
    habitat = habitats_to_adjust,
    service_type = services_to_adjust,
    divisor = custom_divisor,
    stringsAsFactors = FALSE
  )

  # Merge in the custom adjusters, fill NAs with 5
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
