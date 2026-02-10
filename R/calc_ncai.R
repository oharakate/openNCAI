#' Calculate Natural Capital Asset Index (NCAI)
#'
#' Aggregates annual asset matrices into a single time-series index. This function
#' calculates raw totals, a raw index relative to a base year, and a 5-year
#' weighted smoothed index according to the NatureScot methodology.
#'
#' @details
#' The smoothing process uses a 5-year trailing window (current year plus 4 years prior).
#' Weights are applied such that more recent years have a greater influence on
#' the smoothed value.
#'
#' @param total_assets_matrix_list A named list of annual asset data frames
#'   (output from \code{build_all_ncai_matrices}).
#' @param smoothing_weights A numeric vector of weights for the 5-year smoothing
#'   window. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param year_one The year to be used as the index base (100). Defaults to the
#'   first name in the list.
#'
#' @return A data frame with years as row names and three columns:
#'   \item{raw_total}{The sum of all asset values for that year.}
#'   \item{raw_index}{The total indexed against the base year (base = 100).}
#'   \item{smoothed_index}{The 5-year weighted moving average of the raw index.}
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom slider slide_dbl
#' @importFrom utils tail
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

  return(indices_df)
}

#' Calculate NCAI Broken Down by Ecosystem Service Type
#'
#' A wrapper for \code{calc_ncai} that subsets the annual asset matrices by
#' specific columns (ecosystem services) before calculating the index.
#'
#' @param total_assets_matrix_list A named list of annual asset data frames.
#' @param es_label_tree_list A named list where each element is a character
#'   vector of ecosystem service labels (column names).
#' @param ... Additional arguments passed to \code{calc_ncai} (e.g., \code{smoothing_weights}).
#'
#' @return A list of NCAI data frames, one for each ecosystem service group.
#' @export
calc_ncai_by_st <- function(total_assets_matrix_list,
                            es_label_tree_list,
                            ...) {

  lapply(es_label_tree_list, function(subset_labels) {

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
#' @param ... Additional arguments passed to \code{calc_ncai}.
#'
#' @return A list of NCAI data frames, one for each habitat group.
#' @export
calc_ncai_by_bh <- function(total_assets_matrix_list,
                            habitats_label_tree,
                            ...) {

  lapply(habitats_label_tree, function(subset_labels) {

    filtered_matrix_list <- lapply(total_assets_matrix_list, function(m) {
      m[subset_labels, , drop = FALSE]
    })

    calc_ncai(filtered_matrix_list, ...)
  })
}
