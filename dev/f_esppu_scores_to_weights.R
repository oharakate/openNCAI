## FUNCTION esppu_scores_to_weights()
# Takes dataframe object of ESSPU scores (matrix habitats/ecosystem services)
# and converts it to weights by dividing by a common denominator.

esppu_scores_to_weights <- function(
    esppu, # dataframe habitat type / ecosystem service
    divisor = 5, # divisor for calculating weights from scores
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
