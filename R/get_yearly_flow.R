#' Get Yearly Ecosystem Service Flow Rates
#'
#' This high-level function orchestrates the transformation of raw condition scores
#' into normalized Total Yearly Flow (TYF) matrices. It builds the weighted
#' relevance matrices (CIWMs), calculates the total relevance (TIR), and
#' processes every year in the series.
#'
#' @param cirm_list A list of Condition Indicator Relevance Matrices.
#' @param indicator_directory A data frame containing relevance weights for each indicator.
#' @param es_label_tree A named list of ecosystem service labels.
#' @param habitats_label_tree A named list of habitat labels.
#' @param ci_scores A data frame or matrix of raw condition scores (years as rows).
#' @param year_list A vector of years to be processed.
#' @param tir_constant A numeric constant (default 2) to prevent zero-division
#'   and ensure indexing congruency.
#'
#' @return A named list of Total Yearly Flow (TYF) matrices, one for each year
#'   in \code{year_list}.
#'
#' @examples
#' # 1. Load the bundled NatureScot example data
#' data("ns_ci_relevance_matrices", package = "openNCAI")
#' data("ns_indicator_directory", package = "openNCAI")
#' data("ns_es_label_tree", package = "openNCAI")
#' data("ns_habitats_label_tree", package = "openNCAI")
#' data("ns_ci_scores", package = "openNCAI")
#' data("ns_year_list", package = "openNCAI")
#'
#' # 2. Generate Total Yearly Flow (TYF) matrices
#' # This orchestrates the full transformation from raw scores to normalized flow.
#' flow_of_services_ts <- get_yearly_flow(
#'   cirm_list = ns_ci_relevance_matrices,
#'   indicator_directory = ns_indicator_directory,
#'   es_label_tree = ns_es_label_tree,
#'   habitats_label_tree = ns_habitats_label_tree,
#'   ci_scores = ns_ci_scores,
#'   year_list = ns_year_list,
#'   tir_constant = 2
#' )
#'
#' # 3. Inspect the results
#' # Access the flow matrix for a specific year (e.g., the first year in the series)
#' first_year <- ns_year_list[1]
#' print(flow_of_services_ts[[first_year]][1:5, 1:5])
#'
#' @export
get_yearly_flow <- function(cirm_list,
                           indicator_directory,
                           es_label_tree,
                           habitats_label_tree,
                           ci_scores,
                           year_list,
                           tir_constant = 2) {

  # 1. Generate the list of weighted relevance matrices (CIWMs)
  # This weights each indicator according to its importance to specific service groups.
  ciwms_list <- build_ciwm_list(
    cirm_list = cirm_list,
    indicator_directory = indicator_directory,
    es_label_tree = es_label_tree,
    habitats_label_tree = habitats_label_tree
  )

  # 2. Calculate the Total Indicator Relevances (TIR) matrix
  # This serves as the denominator for normalization.
  tir <- calc_tir(
    all_ciwms_list = ciwms_list,
    tir_constant = tir_constant
  )

  # 3. Calculate the Total Yearly Flow (TYF) matrices for all years
  # This combines condition scores with relevances and normalizes the output.
  tyfs_list <- build_all_tyfs(
    raw_cis = ci_scores,
    year_list = year_list,
    ciwms_list = ciwms_list,
    tir = tir,
    tir_constant = tir_constant
  )

  return(tyfs_list)
}
