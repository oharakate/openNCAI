## FUNCTION esppu_scores_to_weights()
# Takes dataframe object of ESSPU scores (matrix habitats/ecosystem services)
# and converts it to weights by dividing by a common denominator, or a matrix
# in shape habitat/ecosystem service of custom divisors.

#' Transform ESPPU Scores to Weights
#'
#' This function converts ecosystem service provision potential per unit
#' scores to weights by dividing by a common denominator, or a matrix of
#' custom divisors per habitat/ecosystem service combination.
#'
#' @param esppu A numeric \code{data.frame} where rows represent habitat types
#' and columns represent ecosystem services and cells denote the potential of
#' the relevant habitat type to provide the relevant ecosystem service.
#' @param divisor A single numeric value by which ES potential scores are
#' divided to calculate the weights. Only used if \code{custom_divisor_matrix}
#' is \code{NULL}.
#' @param custom_divisor_matrix A numeric \code{data.frame} of the same
#' dimensions as \code{esppu}, containing the number by which the score should
#' be divided to generate the weight for each habitat/service
#' combination.
##' @param habitats_label_tree A named list of character vectors where
#' each list name represents a broad habitat category, typically a EUNIS level
#' 1 habitat (e.g. "coastal") and the associated character vector contains the
#' labels or codes of the habitat sub-types - typically EUNIS level two
#' habitats - falling within that category (e.g., c("b1", "b2") or
#' c("coastal_dunes_sandy_shores", "coastal_shingle")). Syntactical names only
#' (no spaces or special characters).
#' The habitats label tree defines the relevant habitats for calculating the
#' NCAI and will be used to label the returned data frame.
#' @param es_label_tree A named list of character vectors where each list
#' name represents a label name for a type group of ecosystem services
#' (e.g. 'provisioning') and the associated character vector contains the
#' labels for the ecosystem services in that group (e.g. 'cultivated_crops').
#' Syntactical names only (no spaces or special characters).
#' The ES label tree defines the relevant ecosystem services for calculating
#' the NCAI and will be used to label the returned data frame.
#'
#' @return A labelled data frame of the same dimensions as ESSPU containing
#' the calculated ESPPU weights.
#'
#' @section Warning:
#' If \code{custom_divisor_matrix} is provided, it must have the same
#' dimensions as \code{esppu}.
#'
#' @export
#'
#' @examples
#' scores <- data.frame(service1 = c(10,5), service2 = c(2,8))
#' hab_labels = c("b1", "b2")
#' es_labels = c("cultivated_crops", water_drinking")
#'
#' # Using a universal divisor
#' esppu_scores_to_weights(scores,
#'   divisor = 10,
#'   habitats_label_tree = hab_labels,
#'   es_label_tree = es_labels
#'   )
#'
#' # Using a custom matrix
#' custom_div <- data.frame(service1 = c(10, 10), service2 = c(5, 5))
#' esppu_scores_to_weights(scores, custom_divisor_matrix = custom_div)
esppu_scores_to_weights <- function(
    esppu,
    divisor = NULL,
    custom_divisor_matrix = NULL,
    habitats_label_tree,
    es_label_tree
) {

  # Make sure a common divisor or custom matrix is provided:
  if (is.null(custom_divisor_matrix) && is.null(divisor)) {
    stop("You must provide either a 'divisor' or a 'custom_divisor_matrix'.")
  }

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

  return(label_ncai_matrix(esppu_aw, habitats_label_tree, es_label_tree))
}
