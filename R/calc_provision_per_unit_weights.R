#' @title Calculate Weights from Provision Per Unit Scores
#'
#' @description Converts scores denoting ecosystem service Provision
#' Per Unit of habitats to weights by dividing by either a common
#' denominator, or a matrix of custom divisors per habitat/ecosystem service
#' combination.
#'
#' @param provision_per_unit_scores A numeric data frame where rows represent habitat types
#' and columns represent ecosystem services and cells denote the exemplary
#' capacity of each habitat type to provide each ecosystem service.
#' @param divisor A single numeric value by which Provision Per Unit scores are
#' divided to calculate the weights, typically the number out of which scores
#' have been awarded. Default value is 5. Alternatively a
#' \code{custom_divisor_matrix} may be specified.
#' @param custom_divisor_matrix Optional. A numeric data frame of the same
#' dimensions as 'provision_per_unit', containing the number by which the score should
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
#' @return A labelled data frame of the same dimensions as
#' \code{provision_per_unit_scores} containing
#' the calculated Provision Per Unit weights.
#'
#' @section Warning:
#' Either \code{divisor} or \code{custom_divisor_matrix} must be provided.
#' If \code{custom_divisor_matrix} is provided, it must have the same
#' dimensions as \code{provision_per_unit}.
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
#' openNCAI:::calc_provision_per_unit_weights(
#'   scores,
#'   divisor = 10,
#'   habitats_label_tree = hab_tree,
#'   es_label_tree = es_tree
#' )
#'
#' # 2. Using a custom matrix with labels
#' custom_div <- data.frame(service1 = c(10, 10), service2 = c(5, 5))
#' openNCAI:::calc_provision_per_unit_weights(scores,
#'   custom_divisor_matrix = custom_div,
#'   habitats_label_tree = hab_tree,
#'   es_label_tree = es_tree
#' )
#'
#' # 3. Running without labels (returns simple data frame)
#' openNCAI:::calc_provision_per_unit_weights(scores, divisor = 10)
calc_provision_per_unit_weights <- function(
    provision_per_unit_scores,
    divisor = 5,
    custom_divisor_matrix = NULL,
    habitats_label_tree = NULL,
    es_label_tree = NULL
) {

  original_rownames <- rownames(provision_per_unit_scores)

  # 1. Ambiguity Check: Only stop if both are provided AND divisor is not the default
  if (!is.null(custom_divisor_matrix) && !is.null(divisor) && divisor != 5) {
    stop("Ambiguous input: Both a custom 'divisor' and a 'custom_divisor_matrix' were provided. Please provide only one.")
  }

  # 2. Logic Selection
  if (is.null(custom_divisor_matrix)) {

    # Ensure divisor exists for the division
    if (is.null(divisor)) {
      stop("You must provide either a 'divisor' or a 'custom_divisor_matrix'.")
    }

    if (divisor == 0) {
      stop("The 'divisor' cannot be zero.")
    }

    provision_per_unit_aw <- provision_per_unit_scores / divisor

  } else {
    # Custom matrix logic...
    if (!all(dim(provision_per_unit_scores) == dim(custom_divisor_matrix))) {
      stop("Dimensions of 'provision_per_unit_scores' and 'custom_divisor_matrix' must match.")
    }
    provision_per_unit_aw <- provision_per_unit_scores / custom_divisor_matrix
  }

  rownames(provision_per_unit_aw) <- original_rownames

  if (!is.null(habitats_label_tree) && !is.null(es_label_tree)) {
    provision_per_unit_aw <- label_ncai_matrix(
      matrix = provision_per_unit_aw,
      habitats_label_tree = habitats_label_tree,
      es_label_tree = es_label_tree
    )
  } else {
    provision_per_unit_aw <- as.data.frame(provision_per_unit_aw)
  }

  return(provision_per_unit_aw)
}
