#' Calculate the Wellbeing Base
#'
#' Scales the Ecosystem Service Potential Base (ESPB) by the Importance Weight
#' for each service to generate the Wellbeing Base. ESPB is expressed as each
#' habitat's contribution as a proportion of the total potential for that
#' service. Label trees may be passed in for optional labelling of the output
#' data frame.
#'
#' @param espb A matrix (or data frame) of habitat (rows) by ecosystem service
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
#' @export
#'
#' @examples
#' # Assuming espb and importance_weights are already calculated:
#' # wellbeing_base_matrix <- calc_wellbeing_base(
#' #   espb = espb_matrix,
#' #   importance_weights = weights_vector,
#' #   habitats_label_tree = habitats_tree,
#' #   es_label_tree = es_tree
#' # )
calc_wellbeing_base <- function(espb,
                                importance_weights,
                                habitats_label_tree = NULL,
                                es_label_tree = NULL
                                ) {

  # Ensure input is a data frame
  wellbeing_base <- as.data.frame(espb)

  # Express ecosystem service provision potential base (ESPB) as proportion of
  # habitat total contribution
  wellbeing_base <- wellbeing_base %>%
    dplyr::mutate(across(everything(), ~ . / sum(., na.rm = TRUE))) %>%
    # Handle cases where column sum was 0 to avoid NaN
    dplyr::mutate(across(everything(), ~ ifelse(is.nan(.), 0, .)))

  # Multiply by importance weights
  wellbeing_base <- as.data.frame(
    mapply(`*`, wellbeing_base, importance_weights, SIMPLIFY = FALSE)
  )

  # Scale to 100
  wellbeing_base <- wellbeing_base * 100

  # Label data frame if label trees are passed in
  if (!is.null(habitats_label_tree) && !is.null(es_label_tree)) {
    wellbeing_base <- label_ncai_matrix(
      matrix = wellbeing_base,
      habitats_label_tree = habitats_label_tree,
      es_label_tree = es_label_tree
    )
  }

  return(wellbeing_base)

}
