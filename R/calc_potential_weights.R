#' @title Calculate Potential Weights from ESPPU Scores
#'
#' @description Converts scores denoting ecosystem service provision potential
#' per unit of habitat extent to weights by dividing by either a common
#' denominator, or a matrix of custom divisors per habitat/ecosystem service
#' combination.
#'
#' @param esppu A numeric data frame where rows represent habitat types
#' and columns represent ecosystem services and cells denote the potential of
#' the relevant habitat type to provide the relevant ecosystem service.
#' @param divisor A single numeric value by which ES potential scores are
#' divided to calculate the weights, typically the number out of which scores
#' have been awarded. Only used if custom_divisor_matrix is NULL.
#' @param custom_divisor_matrix A numeric data frame of the same
#' dimensions as 'esppu', containing the number by which the score should
#' be divided for each habitat/service combination.
#' @param habitats_label_tree A named list of character vectors representing the
#' hierarchy of habitats (as character vectors, typically
#' EUNIS Level 2) within broad habitats (as list object names, typically EUNIS
#' Level 1).
#' Syntactical names only (no spaces or special characters).
#' The habitats label tree is optional and if supplied will be used to label
#' the returned data frame.
#' @param es_label_tree A named list of character vectors representing
#' the hierarchy of ecosystem services (as character vectors) within
#' service type group (as list object names).
#' Syntactical names only (no spaces or special characters).
#' The ES label tree is optional and if supplied will be used to label
#' the returned data frame.
#'
#' @return A labelled data frame of the same dimensions as ESSPU containing
#' the calculated ESPPU weights.
#'
#' @section Warning:
#' Either \code{divisor} or \code{custom_divisor_matrix} must be provided.
#' If \code{custom_divisor_matrix} is provided, it must have the same
#' dimensions as \code{esppu}.
#'
#' @keywords internal
#'
#' @examples
#' # Setup dummy scores
#' # FIX: Column names must match the values in es_tree!
#' scores <- data.frame(
#'   crops = c(10, 5),          # Changed from service1
#'   drinking_water = c(2, 8),  # Changed from service2
#'   row.names = c("b1", "b2")
#' )
#'
#' hab_tree <- list(coastal = c("b1", "b2"))
#' es_tree <- list(provisioning = c("crops", "drinking_water"))
#'
#' # 1. Using a universal divisor with labels
#' openNCAI:::calc_potential_weights(
#'   scores,
#'   divisor = 10,
#'   habitats_label_tree = hab_tree,
#'   es_label_tree = es_tree
#' )
#'
#' # 2. Using a custom matrix with labels
#' custom_div <- data.frame(service1 = c(10, 10), service2 = c(5, 5))
#' openNCAI:::calc_potential_weights(scores,
#'   custom_divisor_matrix = custom_div,
#'   habitats_label_tree = hab_tree,
#'   es_label_tree = es_tree
#' )
#'
#' # 3. Running without labels (returns simple data frame)
#' openNCAI:::calc_potential_weights(scores, divisor = 10)
calc_potential_weights <- function(
    esppu,
    divisor = NULL,
    custom_divisor_matrix = NULL,
    habitats_label_tree = NULL,
    es_label_tree = NULL
) {

  # 1. Store the original row names immediately
  original_rownames <- rownames(esppu)

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
    esppu_aw <- esppu / custom_divisor_matrix
  }

  # 2. Re-assign the row names (arithmetic can strip them)
  rownames(esppu_aw) <- original_rownames

  # Call labelling helper if label trees are passed in
  if (!is.null(habitats_label_tree) && !is.null(es_label_tree)) {
    esppu_aw <- label_ncai_matrix(
      matrix = esppu_aw,
      habitats_label_tree = habitats_label_tree,
      es_label_tree = es_label_tree
    )
  } else {
    # Always return a data frame even if unlabelled
    esppu_aw <- as.data.frame(esppu_aw)
  }

  return(esppu_aw)
}
