#' @title Index matrices and apply smoothing
#'
#' @description This function calculates the raw and smoothed Natural Capital
#' Asset Index from a list of yearly asset matrices, e.g. the NCAI matrices.
#' It anchors the index to a specific baseline year and applies a weighted
#' trailing moving average.
#'
#' @param matrix_list A named list of matrices, where each matrix
#'   represents the values for a specific year, with rows = habitats and
#'   columns = ecosystem service type. The names of the list must be the years.
#' @param smoothing_weights A numeric vector of weights used for the trailing
#'   5-year weighted smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param year_one Character or Numeric. The year used as the baseline
#'   (where index = 100). Defaults to the first name in \code{matrix_list}.
#'   The total in this year must be non-zero.
#'
#' @details
#' \strong{Smoothing and Baseline Years:}
#' The smoothed index is calculated using a 5-year trailing window via a
#' weighted moving average (using the \code{slider} package). If \code{year_one}
#' is set to a year other than the first year of the dataset, the
#' \code{smoothed_index} value for that baseline year will likely not be
#' exactly 100. This is because the smoothing reflects the trend of the
#' preceding 4 years. The \code{raw_index} will always remain anchored at 100
#' for the \code{year_one}.
#'
#'
#'
#' @seealso \code{\link{calc_ncai_subset}}, which checks habitat and
#'   service labels before subsetting the yearly NCAI matrices and indexing
#'   them with this function.
#'
#' @return A data frame with one row per year (years as row names) containing:
#' \itemize{
#'   \item \code{raw_total}: The absolute sum of assets for each year.
#'   \item \code{raw_index}: The index value relative to the baseline year.
#'   \item \code{smoothed_index}: The weighted 5-year trailing smoothed trend.
#' }
#'
#' @importFrom slider slide_dbl
#' @importFrom dplyr mutate
#' @importFrom utils tail
#' @export
#'
#' @examples
#' yearly_matrices <- list(
#'   "2000" = matrix(c(10, 20, 30, 40), nrow = 2),
#'   "2001" = matrix(c(12, 20, 33, 41), nrow = 2),
#'   "2002" = matrix(c(15, 19, 35, 44), nrow = 2)
#' )
#' index_and_smooth(yearly_matrices)
index_and_smooth <- function(matrix_list,
                             smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                             year_one = names(matrix_list)[[1]]) {

  year_one <- as.character(year_one)
  if (length(year_one) != 1 || !year_one %in% names(matrix_list)) {
    stop(errorCondition(
      paste0("year_one must be one of the names of matrix_list, got: ",
             paste(year_one, collapse = ", ")),
      class = "openNCAI_unknown_year"
    ))
  }

  # Get the raw totals.
  yearly_sums <- vapply(matrix_list, sum, numeric(1), na.rm = TRUE)

  # Indexing on year one to give a 'raw' index.
  year_one_val <- as.numeric(yearly_sums[year_one])
  if (year_one_val == 0) {
    stop(errorCondition(
      paste0("The total in year_one (", year_one, ") is zero, so no index ",
             "can be calculated. Check the subset has a non-zero value in ",
             "that year."),
      class = "openNCAI_zero_base"
    ))
  }

  # Define the internal weighted smoothing logic
  weighted_smooth <- function(window_vec) {
    current_weights <- tail(smoothing_weights, length(window_vec))
    current_divisor <- sum(current_weights)
    return(sum(window_vec * current_weights) / current_divisor)
  }

  # Build as a dataframe
  indices_df <- data.frame(
    raw_total = as.numeric(yearly_sums),
    raw_index = (as.numeric(yearly_sums) / year_one_val) * 100
  )
  # Set row names explicitly
  rownames(indices_df) <- names(matrix_list)
  # Apply smoothing
  indices_df <- indices_df |>
    dplyr::mutate(
      smoothed_index = slider::slide_dbl(
        .x = .data$raw_index,
        .f = weighted_smooth,
        .before = 4,
        .complete = FALSE
      )
    )

  # Remind user that in smoothed index year one value may not be
  # exactly 100 if a year other than the first year of the data has
  # been elected as year one.
  smoothed_base_val <- indices_df[year_one, "smoothed_index"]

  if (!is.null(year_one) && abs(smoothed_base_val - 100) > 0.01) {
    message(paste0("Note: Smoothed index at baseline year (", year_one,
                   ") is ", round(smoothed_base_val, 2),
                   " due to the trailing window calculation."))
  }

  return(indices_df)
}

