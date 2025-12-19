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
