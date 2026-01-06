## FUCNTION calc_wb() takes the espb (ES potential per habitat/service type
# combo) and expresses each cell as a proportion of
# the total potential for that service type across all habitats (colSums).
# Next it multiplies in iw the importance weights (result of between and within
# service-type weighting process above)).
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
