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
#' @param provision_per_unit_scores A data frame of Ecosystem Service Provision Potential
#' per Unit.
#' @param provision_per_unit_divisor Numeric. A standard divisor to convert
#'   Provision Per Unit scores to  weights; the number out of which Provision
#'   Per Unit scores have been awarded. Default value is 5.
#'   Alternatively, a \code{custom_divisor_matrix} may be provided.
#' @param custom_divisor_matrix Optional. A matrix of divisors specific to
#'   habitat/service combinations.
#' @param between_importance_scores Scores representing the relative importance
#'   between different ecosystem service types.
#' @param within_importance_scores Scores representing the relative importance
#'   within ecosystem service types.
#' @param ci_relevance_matrices Condition Indicator Relevance Matrices list.
#' @param indicator_directory Directory mapping indicators to services/habitats.
#' @param total_indicator_relevances_constant Numeric. The constant used in the Total Indicator
#'   Relevance calculation. Defaults to 2.
#' @param smoothing_weights Numeric vector of weights for 5-year trailing
#'   smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#' @param return Character. Specifies the object to return. Options include:
#' \itemize{
#'   \item \code{"overall_ncai"}: The standard overall NCAI data frame (default).
#'   \item \code{"by_ecosystem_service_type"}: NCAI broken down by Ecosystem Service Type.
#'   \item \code{"by_broad_habitat"}: NCAI broken down by Broad Habitat.
#'   \item \code{"wellbeing_index"}: The potential wellbeing contribution of the
#'   habitats (before weighting by likely flow of services) over the years,
#'   indexed.
#'   \item \code{"flow_of_es_index"}: The likely flow of ecosystem services (based on
#'   information from condition indicators) over the years,
#'   indexed.
#'   \item \code{"yearly_ncai_matrices"}: The overall NCAI in its unaggregated
#'   form, expressed as yearly matrices of value per habitat/ecosystem service.
#'   \item \code{"yearly_wellbeing_matrices"}: The yearly wellbeing potential in
#'    its unaggregated form, expressed as yearly matrices of value per
#'    habitat/ecosystem service.
#'   \item \code{"yearly_flow_of_es_matrices"}: The yearly likely flow of ecosystem
#'    services in its unaggregated form, expressed as yearly matrices of value
#'    per habitat/ecosystem service.
#'   \item \code{"es_potential_base"}: The Ecosystem Service Potential Base matrix, i.e. the
#'   habitat extent weighted weighted by exemplary provision-per-unit scores in
#'   year one.
#'   \item \code{"wellbeing_potential_base"}: The Wellbeing Potential Base, ie.
#'   year one potential wellbeing matrix.
#'   \item \code{"flow_of_es_base"}: The year one likely flow of services matrix.
#'   \item \code{"everything"}: A named list containing all of the above.
#' }
#'
#' @details
#' \strong{Mandatory Inputs:} Users must provide either an \code{provision_per_unit_divisor}
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
                      provision_per_unit_scores,
                      provision_per_unit_divisor = 5,
                      custom_divisor_matrix = NULL,
                      between_importance_scores,
                      within_importance_scores,
                      ci_relevance_matrices,
                      indicator_directory,
                      total_indicator_relevances_constant = 2,
                      smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0),
                      return =
                        c("overall_ncai",
                          "by_ecosystem_service_type",
                          "by_broad_habitat",
                          "wellbeing_index",
                          "flow_of_es_index",
                          "yearly_ncai_matrices",
                          "yearly_wellbeing_matrices",
                          "flow_of_es_matrices",
                          "es_potential_base",
                          "wellbeing_potential_base",
                          "flow_of_es_base",
                          "everything")) {

  # Helper to show messages about custom weights divisors
  show_divisor_notes <- function() {
    if (!is.null(custom_divisor_matrix)) {
      message("Note: NCAI calculated using a custom divisor matrix for Provision Per Unit weights.")
    } else if (provision_per_unit_divisor != 5) {
      message(paste0("Note: NCAI calculated using a non-standard Provision Per Unit divisor of ",
                     provision_per_unit_divisor, " (Standard template uses 5)."))
    }
  }

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
  # Provision Per Unit scores are converted
  # to weights and multiplied by year one extent data.
  provision_per_unit_weights <- calc_provision_per_unit_weights(
    provision_per_unit_scores = provision_per_unit_scores,
    divisor = provision_per_unit_divisor,
    custom_divisor_matrix = custom_divisor_matrix,
    habitats_label_tree = habitats_label_tree,
    es_label_tree = es_label_tree
  )
  es_potential_base <-  calc_es_potential_base(provision_per_unit_weights = provision_per_unit_weights,
                     habitat_extent = habitat_extent,
                     year_list = year_list,
                     year_one = year_one,
                     habitats_label_tree = habitats_label_tree,
                     es_label_tree = es_label_tree)
  # Return es_potential_base or continue:
  if (return_type == "es_potential_base") {
    show_divisor_notes()
    return(es_potential_base)
    }
  if (return_type == "everything") {
    results$es_potential_base <- es_potential_base
    }


  # 2a. Make the Wellbeing Base.
  # Between- and within-service-type importance scores are converted
  # to weights and multiplied by the ES potential base
  importance_weights <- calc_importance_weights(
    between_scores = between_importance_scores,
    within_scores = within_importance_scores,
    es_label_tree = es_label_tree)

  wellbeing_potential_base <- calc_wellbeing_potential_base(
    es_potential_base = es_potential_base,
    importance_weights = importance_weights,
    habitats_label_tree = habitats_label_tree,
    es_label_tree = es_label_tree
  )
  if (return_type == "wellbeing_potential_base") {
    show_divisor_notes()
    return(wellbeing_potential_base)
    }
  if (return_type == "everything")
    results$wellbeing_potential_base <- wellbeing_potential_base

  # 2b. Make the yearly wellbeing matrices if requested
  if (return_type %in% c("yearly_wellbeing_matrices", "everything")) {
    yearly_wellbeing_matrices <- get_yearly_potential_wellbeing(
      habitat_extent = habitat_extent,
      year_one = year_one,
      wellbeing_potential_base = wellbeing_potential_base,
      as_matrices = TRUE
    )
    if (return_type == "yearly_wellbeing_matrices") {
      show_divisor_notes()
      return(yearly_wellbeing_matrices)
      }
    if (return_type == "everything")
      results$yearly_wellbeing_matrices <- yearly_wellbeing_matrices
  }

  # 2c. Make the yearly wellbeing index if requested
  if (return_type %in% c("wellbeing_index", "everything")) {
    wellbeing_index <- get_yearly_potential_wellbeing(
      habitat_extent = habitat_extent,
      year_one = year_one,
      wellbeing_potential_base = wellbeing_potential_base,
      as_matrices = FALSE
    )
    if (return_type == "wellbeing_index") {
      show_divisor_notes()
      return(wellbeing_index)
      }
    if (return_type == "everything")
      results$wellbeing_index <- wellbeing_index
  }

  # 3. Make the Total Yearly Flow matrices.
  yearly_flow_of_es_matrices <- get_yearly_flow(
    cirm_list = ci_relevance_matrices,
    indicator_directory = indicator_directory,
    es_label_tree = es_label_tree,
    habitats_label_tree = habitats_label_tree,
    ci_scores = ci_scores,
    year_list = year_list,
    total_indicator_relevances_constant = total_indicator_relevances_constant
  )

  # 3a. Return or store full matrices
  if (return_type == "flow_of_es_matrices") {
    show_divisor_notes()
    return(yearly_flow_of_es_matrices)
    }
  if (return_type == "everything") results$yearly_flow_of_es_matrices <- yearly_flow_of_es_matrices

  # Return the specific matrix for the baseline year (flow_of_es_base)
  if (return_type %in% c("flow_of_es_base", "everything")) {
    flow_of_es_base <- yearly_flow_of_es_matrices[[as.character(year_one)]]

    if (return_type == "flow_of_es_base") {
      show_divisor_notes()
      return(flow_of_es_base)
      }
    if (return_type == "everything") results$flow_of_es_base <- flow_of_es_base
  }

  # 3b. Make the Yearly Flow Index
  if (return_type %in% c("flow_of_es_index", "everything")) {
    flow_of_es_index <- index_and_smooth(
      matrix_list = yearly_flow_of_es_matrices,
      year_one = year_one
    )

    if (return_type == "flow_of_es_index") {
      show_divisor_notes()
      return(flow_of_es_index)
      }
    if (return_type == "everything") results$flow_of_es_index <- flow_of_es_index
  }

  # 4. Count the total yearly Natural Capital Assets
  # Multiply Flow by Well-being Base.
  yearly_ncai_matrices <- build_all_ncai_matrices(
    tyf_list = yearly_flow_of_es_matrices,
    wellbeing_potential_base = wellbeing_potential_base,
    habitat_extent = habitat_extent,
    year_one = year_one,
    year_list = year_list,
    habitat_labels = all_habitat_labels
  )
  if (return_type == "yearly_ncai_matrices") {
    show_divisor_notes()
    return(yearly_ncai_matrices)
    }
  if (return_type == "everything")
    results$yearly_ncai_matrices <- yearly_ncai_matrices

  # 5. Index the Natural Capital Assets (overall)
  # The sum of each year matrix is indexed on that of the year one
  # matrix.
  overall_ncai <- index_and_smooth(
    matrix_list = yearly_ncai_matrices,
    smoothing_weights = smoothing_weights,
    year_one = year_one
  )
  if (return_type == "overall_ncai") {
    show_divisor_notes()
    return(overall_ncai)
    }
  if (return_type == "everything")
    results$overall_ncai <- overall_ncai

  # 6. Calculate the index broken down by ecosystem service type
  by_ecosystem_service_type <- calc_ncai_by_st(
    total_assets_matrix_list = yearly_ncai_matrices,
    es_label_tree = es_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "by_ecosystem_service_type") {
    show_divisor_notes()
    return(by_ecosystem_service_type)
    }
  if (return_type == "everything")
    results$by_ecosystem_service_type <- by_ecosystem_service_type

  # 7. Calculate the index broken down by broad habitat
  by_broad_habitat <- calc_ncai_by_bh(
    total_assets_matrix_list = yearly_ncai_matrices,
    habitats_label_tree = habitats_label_tree,
    year_one = year_one,
    smoothing_weights = smoothing_weights
  )
  if (return_type == "by_broad_habitat") {
    show_divisor_notes()
    return(by_broad_habitat)
    }
  if (return_type == "everything") results$by_broad_habitat <- by_broad_habitat

  # Return list of everything if requested
  if (return_type == "everything") {
    # order of return list:
    documented_order <- c(
      "overall_ncai",
      "by_ecosystem_service_type",
      "by_broad_habitat",
      "wellbeing_index",
      "flow_of_es_index",
      "yearly_ncai_matrices",
      "yearly_wellbeing_matrices",
      "yearly_flow_of_es_matrices",
      "es_potential_base",
      "wellbeing_potential_base",
      "flow_of_es_base"
    )
    show_divisor_notes()
    return(results[intersect(documented_order, names(results))])

  } else {
    show_divisor_notes()
    return(overall_ncai)
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
