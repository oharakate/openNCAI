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
