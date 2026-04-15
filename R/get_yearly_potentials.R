#' Worker Function to Calculate Weighted Habitat Extent Time Series
#'
#' An internal helper that multiplies a weight matrix by an indexed habitat
#' extent vector across a time series.
#'
#' @param habitat_extent A matrix or data frame where columns represent years
#'   and rows represent habitats.
#' @param year_one Character or Numeric. The baseline year for indexing.
#' @param weight_matrix A matrix (e.g., ESPB or Wellbeing Base) to be
#'   multiplied by the indexed extent.
#' @param as_matrices Logical. If \code{TRUE}, returns annual matrices;
#'   if \code{FALSE}, returns a smoothed index data frame.
#'
#' @return Depending on \code{as_matrices}, either a named list of matrices
#'   or an indexed data frame.
#' @keywords internal
calc_weighted_habitat_extent <- function(habitat_extent,
                                         year_one,
                                         weight_matrix,
                                         as_matrices = FALSE) {

  years <- colnames(habitat_extent)
  year_one_str <- as.character(year_one)

  extent_indices <- lapply(years,
                           get_habitat_extent_year_vec,
                           year_one = year_one_str,
                           habitat_extent = habitat_extent)

  list_of_matrices <- setNames(
    lapply(extent_indices, function(idx_vec) {
      return(weight_matrix * idx_vec)
    }),
    years)

  if (as_matrices == TRUE) {
    return(list_of_matrices)
  } else {
    index_df <- index_and_smooth(
      matrix_list = list_of_matrices,
      year_one = year_one_str)

    return(index_df)
  }
}

#' Calculate Yearly Potential Provision of Ecosystem Services
#'
#' Calculates the potential provision time series by multiplying indexed
#' habitat extent by the Ecosystem Service Potential Base (ESPB).
#'
#' @inheritParams calc_weighted_habitat_extent
#' @param espb A matrix or data frame of Ecosystem Service Potential Base values.
#'
#' @return Depending on the value of \code{as_matrices}:
#' \itemize{
#'   \item If \code{FALSE}: A data frame with columns \code{raw_total},
#'     \code{raw_index}, and \code{smoothed_index}.
#'   \item If \code{TRUE}: A named list of annual provision matrices.
#' }
#' @export
get_yearly_potential_provision <- function(habitat_extent,
                                           year_one,
                                           espb,
                                           as_matrices = FALSE) {

  return(calc_weighted_habitat_extent(habitat_extent = habitat_extent,
                                      year_one = year_one,
                                      weight_matrix = espb,
                                      as_matrices = as_matrices))
}

#' Calculate Yearly Potential Wellbeing Contribution
#'
#' Calculates the potential wellbeing time series by multiplying indexed
#' habitat extent by the Wellbeing Base.
#'
#' @inheritParams calc_weighted_habitat_extent
#' @param wellbeing_base A matrix or data frame of Wellbeing Base values.
#'
#' @return Depending on the value of \code{as_matrices}:
#' \itemize{
#'   \item If \code{FALSE}: A data frame with columns \code{raw_total},
#'     \code{raw_index}, and \code{smoothed_index}.
#'   \item If \code{TRUE}: A named list of annual wellbeing matrices.
#' }
#' @export
get_yearly_potential_wellbeing <- function(habitat_extent,
                                           year_one,
                                           wellbeing_base,
                                           as_matrices = FALSE) {

  return(calc_weighted_habitat_extent(habitat_extent = habitat_extent,
                                      year_one = year_one,
                                      weight_matrix = wellbeing_base,
                                      as_matrices = as_matrices))
}
