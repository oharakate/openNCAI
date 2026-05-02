#' Get Yearly Ecosystem Service Flow Rates
#'
#' This high-level function orchestrates the transformation of raw condition scores
#' into normalized Total Yearly Flow (TYF) matrices. It builds the weighted
#' relevance matrices (CIWMs), calculates the total indicator relevances, and
#' processes every year in the series.
#'
#' @param cirm_list A list of Condition Indicator Relevance Matrices.
#' @param indicator_directory A data frame containing relevance weights for each indicator.
#' @param es_label_tree A named list of ecosystem service labels.
#' @param habitats_label_tree A named list of habitat labels.
#' @param ci_scores A data frame or matrix of raw condition scores (years as rows).
#' @param year_list A vector of years to be processed.
#' @param total_indicator_relevances_constant A numeric constant (default 2) to prevent zero-division
#'   and ensure indexing congruency.
#'
#' @return A named list of Total Yearly Flow (TYF) matrices, one for each year
#'   in \code{year_list}.
#'
#' @keywords internal
get_yearly_flow <- function(cirm_list,
                           indicator_directory,
                           es_label_tree,
                           habitats_label_tree,
                           ci_scores,
                           year_list,
                           total_indicator_relevances_constant = 2) {

  # 1. Generate the list of weighted relevance matrices (CIWMs)
  # This weights each indicator according to its importance to specific service groups.
  ciwms_list <- build_ciwm_list(
    cirm_list = cirm_list,
    indicator_directory = indicator_directory,
    es_label_tree = es_label_tree,
    habitats_label_tree = habitats_label_tree
  )

  # 2. Calculate the Total Indicator Relevances matrix
  # This serves as the denominator for normalization.
  total_indicator_relevances <- calc_total_indicator_relevances(
    all_ciwms_list = ciwms_list,
    total_indicator_relevances_constant = total_indicator_relevances_constant
  )

  # 3. Calculate the Total Yearly Flow (TYF) matrices for all years
  # This combines condition scores with relevances and normalizes the output.
  tyfs_list <- build_all_tyfs(
    raw_cis = ci_scores,
    year_list = year_list,
    ciwms_list = ciwms_list,
    total_indicator_relevances = total_indicator_relevances,
    total_indicator_relevances_constant = total_indicator_relevances_constant
  )

  return(tyfs_list)
}
