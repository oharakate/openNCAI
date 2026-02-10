#' Build All Total Yearly Flows (TYFs)
#'
#' Orchestrates the full process of generating the Total Yearly Flow matrices for
#' every year in the series. It builds the indicator-specific contribution
#' matrices (YWCCMs) and then aggregates and normalizes them.
#'
#' @param raw_cis A data frame of raw condition indicator scores.
#' @param year_list A vector of all years in the account (e.g., 2000:2022).
#' @param ciwms_list A named list of Condition Indicator Weighting Matrices (CIWMs).
#' @param tir The Total Indicator Relevance matrix (output from \code{calc_tir}).
#' @param tir_constant A numeric value (usually 2) used for normalization.
#'
#' @return A named list of matrices, one for each year in \code{year_list}.
#'   Each matrix represents the aggregated flow of ecosystem services for that year.
#' @export
build_all_tyfs <- function(raw_cis, year_list, ciwms_list, tir, tir_constant) {

  # Call the process for every year in the list
  raw_tyf_list <- lapply(year_list, function(yr) {

    # STEP A: Build all the individual indicator matrices for THIS year
    current_year_ywccms <- build_all_ywccms(
      raw_cis = raw_cis,
      year = yr,
      year_list = year_list,
      ciwms_list = ciwms_list
    )

    # STEP B: Sum them and normalize
    tyf <- build_tyf(
      list_of_ywccms = current_year_ywccms,
      tir = tir,
      tir_constant = tir_constant
    )

    return(tyf)
  })

  names(raw_tyf_list) <- as.character(year_list)
  return(raw_tyf_list)
}

#' Build Yearly Weighted Condition Contribution Matrices (YWCCMs)
#'
#' Generates a list of matrices where each indicator's relevance weighting
#' (CIWM) is multiplied by its indexed condition score for a given year.
#'
#' @param raw_cis A data frame of raw condition indicator scores.
#' @param year The year for which to build the matrices.
#' @param year_list A vector of all years in the account (to establish base year).
#' @param ciwms_list A named list of Condition Indicator Weighting Matrices (CIWMs).
#'
#' @return A named list of matrices (YWCCMs), one for each indicator.
#' @export
build_all_ywccms <- function(raw_cis, year, year_list, ciwms_list) {

  # Iterate through the list of CIWMs to apply the scalar multiplier
  all_ywccms_list <- lapply(seq_along(ciwms_list), function(ci_num) {

    # Get the indexed condition score for this specific indicator
    ci_this_year <- get_yearly_condition(
      raw_cis = raw_cis,
      year_to_get = year,
      ci_num = ci_num,
      year_list = year_list
    )

    # Multiply condition scalar by the corresponding weighted relevance matrix
    ywccm <- ciwms_list[[ci_num]] * ci_this_year

    return(ywccm)
  })

  names(all_ywccms_list) <- names(ciwms_list)
  return(all_ywccms_list)
}

# --- Internal Helper Functions ---

#' Build Total Yearly Flow (TYF) for a Single Year
#'
#' Aggregates a list of YWCCMs and normalizes the sum against the TIR matrix.
#'
#' @param list_of_ywccms A list of Yearly Weighted Condition Contribution Matrices.
#' @param tir The Total Indicator Relevance matrix.
#' @param tir_constant Numeric. Added to denominator for stability and numerator for indexing.
#'
#' @return A numeric matrix for one year.
#' @keywords internal
build_tyf <- function(list_of_ywccms, tir, tir_constant) {

  # Aggregation across the list of indicators
  sum_ywccms <- Reduce("+", list_of_ywccms)

  # Normalize: (Sum + Constant * 100) / (Sum of Weights + Constant)
  tyf <- (sum_ywccms + (100 * tir_constant)) / tir

  return(tyf)
}

#' Extract Indexed Condition Score for a Specific Indicator
#'
#' Retrieves and indexes (base 100) a condition score for a single indicator.
#'
#' @param raw_cis Data frame of raw condition scores.
#' @param year_to_get Target year.
#' @param ci_num Indicator column index or name.
#' @param year_list Vector of years (index 1 is base year).
#'
#' @return A numeric indexed score.
#' @keywords internal
get_yearly_condition <- function(raw_cis, year_to_get, ci_num, year_list) {

  # Row lookup by character (year) and column by index/name
  raw_cond_score <- raw_cis[as.character(year_to_get), ci_num]
  year_one_score <- raw_cis[as.character(year_list[1]), ci_num]

  # Index calculation with numeric stripping
  indexed_cond_score <- (as.numeric(raw_cond_score) / as.numeric(year_one_score)) * 100

  return(indexed_cond_score)
}
