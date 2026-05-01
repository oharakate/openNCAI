#' Calculate Habitat Extent (Indexed) for a Target Year
#'
#' This helper function extracts habitat extent vectors for a specific target year
#' and a baseline year, then calculates a percentage-based index.
#'
#' @param target_year The year to be indexed (numeric or character).
#' @param year_one The baseline year used as the denominator (numeric or character).
#' @param habitat_extent A matrix or data frame where columns represent years
#'   and rows represent habitat types/locations.
#'
#' @return A numeric vector of the same length as the number of rows in
#'   \code{habitat_extent}, representing the extent of each habitat in the
#'   \code{target_year} as a percentage of its extent in \code{year_one}.
#'
#' @details
#' The function performs the calculation: \cr
#' \eqn{(Target Extent / Baseline Extent) * 100}
#'
#' @keywords internal
get_habitat_extent_year_vec <- function(target_year, year_one, habitat_extent) {

  # Convert to characters for safe indexing
  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors
  extent_target_vec <- habitat_extent[, target_str]
  extent_origin_vec <- habitat_extent[, origin_str]

  # Index the habitat extent values (Target / Origin * 100)
  extent_index <- (extent_target_vec / extent_origin_vec * 100)

  return(extent_index)

}
