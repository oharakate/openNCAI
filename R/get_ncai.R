#' @title Main User Interface for the Natural Capital Asset Index (NCAI)
#'
#' @description This is the primary function for calculating the NCAI.
#' It processes habitat extent, condition scores, and importance weights
#' through the full NCAI pipeline, with options to return intermediate matrices
#'  or specific breakdowns.
#'
#' @param habitat_extent A data frame of habitat area/extent per year.
#' @param ci_score_matrix A matrix of condition indicator scores.
#' @param habitats_label_tree A named list defining the hierarchy of habitats.
#' @param es_label_tree A named list defining the hierarchy of ecosystem
#' services.
#' @param year_list A vector (character or numeric) of years for the index.
#' @param year_one Optional. The baseline year for indexing (where index = 100).
#'   Defaults to the first year in \code{year_list}.
#' @param esppu_scores A data frame of Ecosystem Service Provision Potential
#' per Unit.
#' @param esppu_divisor Numeric. A standard divisor to convert ESPPU scores to
#'   weights. Used if \code{custom_divisor_matrix} is NULL.
#' @param custom_divisor_matrix Optional. A matrix of divisors specific to
#'   habitat/service combinations.
#' @param between_importance_scores Scores representing the relative importance
#'   between different ecosystem service types.
#' @param within_importance_scores Scores representing the relative importance
#'   within ecosystem service types.
#' @param cirms_list Condition Indicator Relevance Matrices list.
#' @param indicator_directory Directory mapping indicators to services/habitats.
#' @param tir_constant Numeric. The constant used in the Total Indicator
#'   Relevance (TIR) calculation. Defaults to 2.
#' @param smoothing_weights Numeric vector of weights for 5-year trailing
#'   smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param return Character. Specifies the object to return. Options include:
#' \itemize{
#'   \item \code{"overall"}: The standard overall NCAI data frame (default).
#'   \item \code{"index_by_st"}: NCAI broken down by Ecosystem Service Type.
#'   \item \code{"index_by_bh"}: NCAI broken down by Broad Habitat.
#'   \item \code{"yearly_asset_matrices"}: A list of matrices containing asset
#'     values for every year.
#'   \item \code{"espb"}: The Ecosystem Service Potential Base matrix.
#'   \item \code{"wellbeing_base"}: The Wellbeing Base matrix.
#'   \item \code{"yearly_flow_matrices"}: A list of yearly condition/flow
#'   matrices.
#'   \item \code{"everything"}: A named list containing all of the above.
#' }
#'
#' @details
#' \strong{Mandatory Inputs:} Users must provide either an \code{esppu_divisor}
#' or a \code{custom_divisor_matrix} to convert potential scores into weights.
#'
#' \strong{Smoothing and Baseline Years:}
#' The smoothed index is calculated using a 5-year trailing window.
#' If \code{year_one}
#' is set to a year other than the first year of the dataset, the
#' \code{smoothed_index} value for that baseline year will likely not be
#' exactly 100. This is because the smoothing reflects the trend of the
#' preceding 4 years. The \code{raw_index} will always remain anchored at 100
#' for the \code{year_one}.
#'
#' @return An object of the type specified by the \code{return} argument.
#'   Typically a data frame or a named list of NCAI components.
#' @export

get_ncai <-  function(habitat_extent,
                      ci_score_matrix,
                      habitats_label_tree,
                      es_label_tree,
                      year_list,
                      year_one = NULL,
                      esppu_scores,
                      esppu_divisor = NULL,
                      custom_divisor_matrix = NULL,
                      between_importance_scores,
                      within_importance_scores,
                      cirms_list,
                      indicator_directory,
                      tir_constant = 2,
                      smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                      return =
                        c("overall",
                          "index_by_st",
                          "index_by_bh",
                          "yearly_asset_matrices",
                          "espb",
                          "wellbeing_base",
                          "yearly_flow_matrices",
                          "everything")) {

  # Assign return type:
  return_type <- match.arg(return)

  # Create derived objects:
  if (is.null(year_one)) {
    year_one <- as.character(year_list[[1]])
  } else {
    year_one <- as.character(year_one)
  }

  all_habitat_labels <- unlist(habitats_label_tree, use.names = FALSE)

  # Initialise a list of objects which may be returned:
  results <- list()

  # 1. Make the Ecosystem Service Potential Base.
  # Ecosystem Service Potential Per Unit (ESPPU) scores are converted
  # to weights and multiplied by year one extent data.
  esppu_weights <- calc_potential_weights(
    esppu = esppu_scores,
    divisor = esppu_divisor,
    custom_divisor_matrix = custom_divisor_matrix,
    habitats_label_tree = habitats_label_tree,
    es_label_tree = es_label_tree
  )
  espb <-  calc_espb(esppu_weights = esppu_weights,
                     habitat_extent = habitat_extent,
                     year_list = year_list,
                     year_one = year_one,
                     habitats_label_tree = habitats_label_tree,
                     es_label_tree = es_label_tree)
  # Return espb or continue:
  if (return_type == "espb") return(espb)
  if (return_type == "everything") results$espb <- espb


  # 2. Make the Wellbeing Base.
  # Between- and within-service-type importance scores are converted
  # to weights and multiplied by the ESPB
  importance_weights <- calc_importance_weights(
    between_scores = between_importance_scores,
    within_scores = within_importance_scores,
    es_label_tree = es_label_tree)

  wellbeing_base <- calc_wellbeing_base(
    espb = espb,
    importance_weights = importance_weights,
    habitats_label_tree = habitats_label_tree,
    es_label_tree = es_label_tree
  )
  if (return_type == "wellbeing_base") return(wellbeing_base)
  if (return_type == "everything")
    results$wellbeing_base <- wellbeing_base

  # 3. Make the Total Yearly Flow matrices.
  # Condition scores are weighted by relevance and combined into
  # yearly matrices of flow rate by habitat/service combination.
  yearly_flow_matrices <- calc_flow_rate(
    cirm_list = cirms_list,
    indicator_directory = indicator_directory,
    es_label_tree = es_label_tree,
    habitats_label_tree = habitats_label_tree,
    ci_score_matrix = ci_score_matrix,
    year_list = year_list,
    tir_constant = tir_constant
  )
  if (return_type == "yearly_flow_matrices")
    return(yearly_flow_matrices)
  if (return_type == "everything")
    results$yearly_flow_matrices <- yearly_flow_matrices

  # 4. Count the total yearly Natural Capital Assets
  # Multiply Flow by Well-being Base.
  yearly_asset_matrices <- build_all_ncai_matrices(
    tyf_list = yearly_flow_matrices,
    wellbeing_base = wellbeing_base,
    habitat_extent = habitat_extent,
    year_one = year_one,
    year_list = year_list,
    habitat_labels = all_habitat_labels
  )
  if (return_type == "yearly_asset_matrices")
    return(yearly_asset_matrices)
  if (return_type == "everything")
    results$yearly_asset_matrices <- yearly_asset_matrices

  # 5. Index the Natural Capital Assets (overall)
  # The sum of each year matrix is indexed on that of the year one
  # matrix.
  overall_index <- calc_ncai(
    total_assets_matrix_list = yearly_asset_matrices,
    smoothing_weights = smoothing_weights,
    year_one = year_one
  )
  if (return_type == "overall") return(overall_index)
  if (return_type == "everything")
    results$overall_index <- overall_index

  # 6. Calculate the index broken down by ecosystem service type
  index_by_st <- calc_ncai_by_st(
    total_assets_matrix_list = yearly_asset_matrices,
    es_label_tree = es_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "index_by_st") return(index_by_st)
  if (return_type == "everything")
    results$index_by_st <- index_by_st

  # 7. Calculate the index broken down by broad habitat
  index_by_bh <- calc_ncai_by_bh(
    total_assets_matrix_list = yearly_asset_matrices,
    habitats_label_tree = habitats_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "index_by_bh") return(index_by_bh)
  if (return_type == "everything") results$index_by_bh <- index_by_bh

  # Return list of everything if requested
  if (return_type == "everything") {
    return(results)
  } else {
    return(overall_index)
  }
}
