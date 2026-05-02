#'
#' Scales the Ecosystem Service (ES) Potential Base by the Importance Weight
#' for each service to generate the Wellbeing Base. ES Potential Base is expressed as each
#' habitat's contribution as a proportion of the total potential for that
#' service. Label trees may be passed in for optional labelling of the output
#' data frame.
#'
#' @param es_potential_base A matrix (or data frame) of habitat (rows) by ecosystem service
#' (columns) containing weights to denote the potential of each habitat to
#' provide each ecosystem service, calculated for the baseline year (Year One).
#' @param importance_weights A vector of weights denoting the importance of
#' each ecosystem service in the area of interest.
#' @param habitats_label_tree A named list of character vectors representing the
#' hierarchy of habitats (as character vectors, typically
#' EUNIS Level 2) within broad habitats (as list object names, typically EUNIS
#' Level 1). Syntactical names only (no spaces or special characters).
#' The habitats label tree is optional and if supplied will be used to label
#' the returned data frame.
#' @param es_label_tree A named list of character vectors representing
#' the hierarchy of ecosystem services (as character vectors) within
#' service type group (as list object names).
#' Syntactical names only (no spaces or special characters).
#' The ES label tree is optional and if supplied will be used to label the
#' returned data frame.
#'
#' @return A data frame representing the Wellbeing Base.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr across everything
#'
#' @keywords internal
#'
#' @examples
#' # Assuming es_potential_base and importance_weights are already calculated:
#' # wellbeing_potential_base_matrix <- calc_wellbeing_potential_base(
#' #   es_potential_base = es_potential_base_matrix,
#' #   importance_weights = weights_vector,
#' #   habitats_label_tree = habitats_tree,
#' #   es_label_tree = es_tree
#' # )
calc_wellbeing_potential_base <- function(es_potential_base,
                                importance_weights,
                                habitats_label_tree = NULL,
                                es_label_tree = NULL
) {

  # Store original names because dplyr/mapply operations often strip them
  orig_hab_names <- rownames(es_potential_base)

  # Ensure input is a data frame
  wellbeing_potential_base <- as.data.frame(es_potential_base)

  # Express ES Potential Base as proportion of
  # habitat total contribution
  wellbeing_potential_base <- wellbeing_potential_base %>%
    dplyr::mutate(across(everything(), ~ . / sum(., na.rm = TRUE))) %>%
    # Handle cases where column sum was 0 to avoid NaN
    dplyr::mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))

  # Multiply by importance weights
  wellbeing_potential_base <- as.data.frame(
    mapply(`*`, wellbeing_potential_base, importance_weights, SIMPLIFY = FALSE)
  )

  # Scale to 100
  wellbeing_potential_base <- wellbeing_potential_base * 100

  # Restore row names before calling the labeler or returning
  rownames(wellbeing_potential_base) <- orig_hab_names

  # Label data frame if label trees are passed in
  if (!is.null(habitats_label_tree) && !is.null(es_label_tree)) {
    wellbeing_potential_base <- label_ncai_matrix(
      matrix = wellbeing_potential_base,
      habitats_label_tree = habitats_label_tree,
      es_label_tree = es_label_tree
    )
  }

  return(wellbeing_potential_base)
}
