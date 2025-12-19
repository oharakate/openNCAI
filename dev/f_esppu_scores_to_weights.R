## FUNCTION esppu_scores_to_weights()
# Takes matrix of ESSPU scores and converts it to weights
esppu_scores_to_weights <- function(esppu, custom_weight_matrix = 5) {

  esppu_mat <- as.matrix(esppu)
  esppu_aw  <- (esppu_mat / as.matrix(custom_weight_matrix)) %>%
    as.data.frame()

  return(esppu_aw)
}
