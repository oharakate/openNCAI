#' Calculate Total Indicator Relevances
#'
#' Aggregates a list of weighted condition indicator matrices into a single
#' matrix representing the total relevance of all selected indicators to
#' each habitat-service combination.
#'
#' @details
#' The function uses \code{Reduce("+", ...)} to perform element-wise addition
#' across all matrices in the input list. A constant is then added to every
#' cell to ensure that subsequent divisions (e.g., when calculating the NCAI)
#' do not encounter zeros.
#'
#' @param all_ciwms_list A list of Condition Indicator Weighting Matrices
#' (CIWMs). Each element in the list must be a data frame
#' where rows correspond to habitats and columns to ecosystem services.
#' @param total_indicator_relevances_constant A numeric value added to the aggregated total prevent
#' zero-division errors in later calculations.
#'
#' @return A data frame of numeric values representing the summed relevance
#' scores plus the constant, with the same dimensions as the input matrices.
#'
#' @keywords internal
calc_total_indicator_relevances <- function(all_ciwms_list, total_indicator_relevances_constant) {

  if (length(all_ciwms_list) == 0) stop("all_ciwms_list is empty.")

  total_indicator_relevances <- Reduce("+", all_ciwms_list)
  total_indicator_relevances <- total_indicator_relevances + total_indicator_relevances_constant

  return(total_indicator_relevances)
}
