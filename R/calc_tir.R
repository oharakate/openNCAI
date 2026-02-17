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
#' @param tir_constant A numeric value added to the aggregated total prevent
#' zero-division errors in later calculations.
#'
#' @return A data frame of numeric values representing the summed relevance
#' scores plus the constant, with the same dimesions as the input matrices.
#'
#' @keywords internal
#' # Mock list of two matrices
#' mat1 <- matrix(1, nrow = 2, ncol = 2)
#' mat2 <- matrix(0.5, nrow = 2, ncol = 2)
#' ciwm_list <- list(mat1, mat2)
#'
#' # Calculate TIR with a constant of 2
#' calc_tir(ciwm_list, tir_constant = 2)
calc_tir <- function(all_ciwms_list, tir_constant) {

  if (length(all_ciwms_list) == 0) stop("all_ciwms_list is empty.")

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + tir_constant

  return(tir)
}
calc_tir <- function(all_ciwms_list, tir_constant) {

  tir <- Reduce("+", all_ciwms_list)
  tir <- tir + tir_constant

  return(tir)
}
