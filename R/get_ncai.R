#' @title Main User Interface for the Natural Capital Asset Index (NCAI)
#'
#' @description This is the primary function for calculating the NCAI.
#' It processes habitat extent, condition scores, and importance weights
#' through the full NCAI pipeline, with options to return intermediate matrices
#'  or specific breakdowns.
#'
#' @param habitat_extent A data frame of habitat area/extent per year.
#' @param ci_scores A matrix of condition indicator scores.
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
#' @param ci_relevance_matrices Condition Indicator Relevance Matrices list.
#' @param indicator_directory Directory mapping indicators to services/habitats.
#' @param tir_constant Numeric. The constant used in the Total Indicator
#'   Relevance (TIR) calculation. Defaults to 2.
#' @param smoothing_weights Numeric vector of weights for 5-year trailing
#'   smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param return Character. Specifies the object to return. Options include:
#' \itemize{
#'   \item \code{"overall_index"}: The standard overall NCAI data frame (default).
#'   \item \code{"by_ecosystem_service_type"}: NCAI broken down by Ecosystem Service Type.
#'   \item \code{"by_broad_habitat"}: NCAI broken down by Broad Habitat.
#'   \item \code{"wellbeing_index"}: The potential wellbeing contribution of the
#'   habitats (before weighting by likely flow of services) over the years,
#'   indexed.
#'   \item \code{"flow_index"}: The likely flow of ecosystem services (based on
#'   information from condition indicators) over the years,
#'   indexed.
#'   \item \code{"yearly_asset_matrices"}: The overall NCAI in its unaggregated
#'   form, expressed as yearly matrices of value per habitat/ecosystem service.
#'   \item \code{"yearly_wellbeing_matrices"}: The yearly potential wellbeing in
#'    its unaggregated form, expressed as yearly matrices of value per
#'    habitat/ecosystem service.
#'   \item \code{"yearly_flow_matrices"}: The yearly likely flow of ecosystem
#'    services in its unaggregated form, expressed as yearly matrices of value
#'    per habitat/ecosystem service.
#'   \item \code{"espb"}: The Ecosystem Service Potential Base matrix, i.e. the
#'   habitat extent weighted weighted by exemplary provision-per-unit scores in
#'   year one.
#'   \item \code{"wellbeing_base"}: The Wellbeing Base, ie. year one potential
#'   wellbeing matrix.
#'   \item \code{"flow_base"}: The year one likely flow of services matrix.
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
                      ci_scores,
                      habitats_label_tree,
                      es_label_tree,
                      year_list,
                      year_one = NULL,
                      esppu_scores,
                      esppu_divisor = NULL,
                      custom_divisor_matrix = NULL,
                      between_importance_scores,
                      within_importance_scores,
                      ci_relevance_matrices,
                      indicator_directory,
                      tir_constant = 2,
                      smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                      return =
                        c("overall_index",
                          "by_ecosystem_service_type",
                          "by_broad_habitat",
                          "wellbeing_index",
                          "flow_index",
                          "yearly_asset_matrices",
                          "yearly_wellbeing_matrices",
                          "flow_matrices",
                          "espb",
                          "wellbeing_base",
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


  # 2a. Make the Wellbeing Base.
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

  # 2b. Make the yearly wellbeing matrices if requested
  if (return_type %in% c("yearly_wellbeing_matrices", "everything")) {
    yearly_wellbeing_matrices <- get_yearly_potential_wellbeing(
      habitat_extent = habitat_extent,
      year_one = year_one,
      wellbeing_base = wellbeing_base,
      as_matrices = TRUE
    )
    if (return_type == "yearly_wellbeing_matrices")
      return(yearly_wellbeing_matrices)
    if (return_type == "everything")
      results$yearly_wellbeing_matrices <- yearly_wellbeing_matrices
  }

  # 2c. Make the yearly wellbeing index if requested
  if (return_type %in% c("wellbeing_index", "everything")) {
    wellbeing_index <- get_yearly_potential_wellbeing(
      habitat_extent = habitat_extent,
      year_one = year_one,
      wellbeing_base = wellbeing_base,
      as_matrices = FALSE
    )
    if (return_type == "wellbeing_index")
      return(wellbeing_index)
    if (return_type == "everything")
      results$wellbeing_index <- wellbeing_index
  }

  # 3. Make the Total Yearly Flow matrices.
  yearly_flow_matrices <- get_yearly_flow(
    cirm_list = ci_relevance_matrices,
    indicator_directory = indicator_directory,
    es_label_tree = es_label_tree,
    habitats_label_tree = habitats_label_tree,
    ci_scores = ci_scores,
    year_list = year_list,
    tir_constant = tir_constant
  )

  # 3a. Return or store full matrices
  if (return_type == "flow_matrices") return(yearly_flow_matrices)
  if (return_type == "everything") results$yearly_flow_matrices <- yearly_flow_matrices

  # NEW: Return the specific matrix for the baseline year (flow_base)
  if (return_type %in% c("flow_base", "everything")) {
    flow_base <- yearly_flow_matrices[[as.character(year_one)]]

    if (return_type == "flow_base") return(flow_base)
    if (return_type == "everything") results$flow_base <- flow_base
  }

  # 3b. Make the Yearly Flow Index
  if (return_type %in% c("flow_index", "everything")) {
    flow_index <- index_and_smooth(
      matrix_list = yearly_flow_matrices,
      year_one = year_one
    )

    if (return_type == "flow_index") return(flow_index)
    if (return_type == "everything") results$flow_index <- flow_index
  }

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
  overall_index <- index_and_smooth(
    matrix_list = yearly_asset_matrices,
    smoothing_weights = smoothing_weights,
    year_one = year_one
  )
  if (return_type == "overall_index") return(overall_index)
  if (return_type == "everything")
    results$overall_index <- overall_index

  # 6. Calculate the index broken down by ecosystem service type
  by_ecosystem_service_type <- calc_ncai_by_st(
    total_assets_matrix_list = yearly_asset_matrices,
    es_label_tree = es_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "by_ecosystem_service_type") return(by_ecosystem_service_type)
  if (return_type == "everything")
    results$by_ecosystem_service_type <- by_ecosystem_service_type

  # 7. Calculate the index broken down by broad habitat
  by_broad_habitat <- calc_ncai_by_bh(
    total_assets_matrix_list = yearly_asset_matrices,
    habitats_label_tree = habitats_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "by_broad_habitat") return(by_broad_habitat)
  if (return_type == "everything") results$by_broad_habitat <- by_broad_habitat

  # Return list of everything if requested
  if (return_type == "everything") {
    # order of return list:
    documented_order <- c(
      "overall_index",
      "by_ecosystem_service_type",
      "by_broad_habitat",
      "wellbeing_index",
      "flow_index",
      "yearly_asset_matrices",
      "yearly_wellbeing_matrices",
      "yearly_flow_matrices",
      "espb",
      "wellbeing_base",
      "flow_base"
    )
    return(results[intersect(documented_order, names(results))])

  } else {
    return(overall_index)
  }
}


#' Calculate NCAI Broken Down by Ecosystem Service Type
#'
#' Calculate the index subsetted by ecosystem service type.
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

    index_and_smooth(filtered_matrix_list, ...)
  })
}

#' Calculate NCAI Broken Down by Broad Habitat
#'
#' Calculates the index subsetted by broad habitat
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

    index_and_smooth(filtered_matrix_list, ...)
  })
}
