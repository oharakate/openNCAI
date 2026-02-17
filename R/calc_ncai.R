#' @title Calculate the Natural Capital Asset Index (NCAI)
#'
#' @description This function calculates the raw and smoothed Natural Capital
#' Asset Index from a list of yearly asset matrices. It anchors the index
#' to a specific baseline year and applies a weighted trailing moving average.
#'
#' @param total_assets_matrix_list A named list of matrices, where each matrix
#'   represents the total assets for a specific year. The names of the list
#'   must correspond to the years (e.g., "2000", "2001").
#' @param smoothing_weights A numeric vector of weights used for the trailing
#'   5-year weighted smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param year_one Character or Numeric. The year used as the baseline
#'   (where index = 100). Defaults to the first name in
#'   \code{total_assets_matrix_list}.
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
#' @return A data frame containing:
#' \itemize{
#'   \item \code{raw_total}: The absolute sum of assets for each year.
#'   \item \code{raw_index}: The index value relative to the baseline year.
#'   \item \code{smoothed_index}: The weighted 5-year trailing smoothed trend.
#' }
#'
#' @importFrom slider slide_dbl
#' @importFrom dplyr mutate
#' @importFrom utils tail
#' @keywords internal
calc_ncai <- function(total_assets_matrix_list,
                      smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                      year_one = names(total_assets_matrix_list)[[1]]) {

  # Get the raw totals.
  yearly_sums <- vapply(total_assets_matrix_list, sum, numeric(1), na.rm = TRUE)

  # Indexing on year one to give a 'raw' index.
  year_one_val <- as.numeric(yearly_sums[year_one])

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
  rownames(indices_df) <- names(total_assets_matrix_list)
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

#' Calculate NCAI Broken Down by Ecosystem Service Type
#'
#' A wrapper for \code{calc_ncai} that subsets the annual asset matrices by
#' specific columns (ecosystem services) before calculating the index.
#'
#' @param total_assets_matrix_list A named list of annual asset data frames.
#' @param es_label_tree A named list where each element is a character
#'   vector of ecosystem service labels (column names).
#' @param year_one Optional: the year to index around. Default is the first
#' year of the \code{year_list}.
#' @param ... Additional arguments passed to \code{calc_ncai} (e.g., \code{smoothing_weights}).
#'
#' @return A list of NCAI data frames, one for each ecosystem service group.
#' @keywords internal
calc_ncai_by_st <- function(total_assets_matrix_list,
                            es_label_tree,
                            year_one = NULL,
                            ...) {

  lapply(es_label_tree, function(subset_labels) {

    filtered_matrix_list <- lapply(total_assets_matrix_list, function(m) {
      m[, subset_labels, drop = FALSE]
    })

    calc_ncai(filtered_matrix_list, ...)
  })
}

#' Calculate NCAI Broken Down by Broad Habitat
#'
#' A wrapper for \code{calc_ncai} that subsets the annual asset matrices by
#' specific rows (habitats) before calculating the index.
#'
#' @param total_assets_matrix_list A named list of annual asset data frames.
#' @param habitats_label_tree A named list where each element is a character
#'   vector of habitat labels (row names).
#' @param year_one Optional: year to index around. Default is year one of the
#' \code{year_list}.
#' @param ... Additional arguments passed to \code{calc_ncai}.
#'
#' @return A list of NCAI data frames, one for each habitat group.
#' @keywords internal
calc_ncai_by_bh <- function(total_assets_matrix_list,
                            habitats_label_tree,
                            year_one = NULL,
                            ...) {

  lapply(habitats_label_tree, function(subset_labels) {

    filtered_matrix_list <- lapply(total_assets_matrix_list, function(m) {
      m[subset_labels, , drop = FALSE]
    })

    calc_ncai(filtered_matrix_list, ...)
  })
}
