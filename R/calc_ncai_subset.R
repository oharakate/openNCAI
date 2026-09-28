#' @title Calculate the NCAI for a Custom Subset of Habitats and Services
#'
#' @description Indexes the NCAI over any combination of habitats (rows of
#' the yearly NCAI matrices) and ecosystem services (columns), for example
#' cultural services from woodland habitats. Labels are checked against the
#' matrices before indexing, so a mistyped label stops with an error rather
#' than being silently left out of the total.
#'
#' @param yearly_ncai_matrices A named list of yearly NCAI matrices, as
#'   returned by \code{get_ncai(return = "yearly_ncai_matrices")}. The names
#'   of the list must be the years.
#' @param habitats Optional. A character vector of habitat labels (row names)
#'   to include. May also contain group names from \code{habitats_label_tree},
#'   which are expanded to every habitat in that group. Defaults to all
#'   habitats.
#' @param services Optional. A character vector of ecosystem service labels
#'   (column names) to include. May also contain group names from
#'   \code{es_label_tree}, which are expanded to every service in that group.
#'   Defaults to all services.
#' @param habitats_label_tree Optional. A named list defining the hierarchy of
#'   habitats. Needed only if \code{habitats} contains group names.
#' @param es_label_tree Optional. A named list defining the hierarchy of
#'   ecosystem services. Needed only if \code{services} contains group names.
#' @param year_one The baseline year for indexing (where index = 100).
#'   Defaults to the first year in \code{yearly_ncai_matrices}. Set this to
#'   match the \code{year_one} given to \code{get_ncai()} if that was not the
#'   first year.
#' @param smoothing_weights Numeric vector of weights for 5-year trailing
#'   smoothing. Defaults to \code{c(0.2, 0.4, 0.6, 0.8, 1.0)}.
#'
#' @details
#' A habitat or service that is selected more than once, directly or through
#' a group, is counted once.
#'
#' The total of the selected cells in \code{year_one} must be non-zero, as
#' the index is expressed relative to it.
#'
#' @return A data frame with one row per year (years as row names) and
#'   columns \code{raw_total}, \code{raw_index} and \code{smoothed_index}, as
#'   returned by \code{\link{index_and_smooth}}.
#'
#' @seealso \code{\link{get_ncai}}, \code{\link{index_and_smooth}}
#'
#' @export
#'
#' @examples
#' yearly_ncai_matrices <- get_ncai(
#'   habitat_extent = ns_habitat_extent,
#'   ci_scores = ns_ci_scores,
#'   habitats_label_tree = ns_habitats_label_tree,
#'   es_label_tree = ns_es_label_tree,
#'   year_list = ns_year_list,
#'   provision_per_unit_scores = ns_provision_per_unit_scores,
#'   custom_divisor_matrix = ns_custom_divisor_matrix,
#'   between_importance_scores = ns_between_importance_scores,
#'   within_importance_scores = ns_within_importance_scores,
#'   ci_relevance_matrices = ns_ci_relevance_matrices,
#'   indicator_directory = ns_indicator_directory,
#'   return = "yearly_ncai_matrices"
#' )
#'
#' # Cultural services from woodland habitats, selected by group name
#' calc_ncai_subset(
#'   yearly_ncai_matrices,
#'   habitats = "g_woodland_forest_and_other_wooded_land",
#'   services = "cultural",
#'   habitats_label_tree = ns_habitats_label_tree,
#'   es_label_tree = ns_es_label_tree
#' )
#'
#' # All services from broadleaved woodland only
#' calc_ncai_subset(
#'   yearly_ncai_matrices,
#'   habitats = "g1_broadleaved_deciduous_woodland"
#' )
calc_ncai_subset <- function(yearly_ncai_matrices,
                             habitats = NULL,
                             services = NULL,
                             habitats_label_tree = NULL,
                             es_label_tree = NULL,
                             year_one = names(yearly_ncai_matrices)[[1]],
                             smoothing_weights = c(0.2, 0.4, 0.6, 0.8, 1.0)) {

  expand_groups <- function(labels, label_tree) {
    unique(unlist(lapply(labels, function(lab) {
      if (lab %in% names(label_tree)) label_tree[[lab]] else lab
    }), use.names = FALSE))
  }

  check_labels <- function(labels, available, what, tree_arg) {
    unknown <- setdiff(labels, available)
    if (length(unknown) > 0) {
      stop(errorCondition(
        paste0("Unknown ", what, " label(s): ", paste(unknown, collapse = ", "),
               ". Labels must be ", what, " names in yearly_ncai_matrices ",
               "or group names in ", tree_arg, "."),
        class = "openNCAI_unknown_label"
      ))
    }
  }

  # Labels are checked against every year so that no year can silently
  # drop a selected cell.
  habitat_labels <- Reduce(intersect, lapply(yearly_ncai_matrices, rownames))
  service_labels <- Reduce(intersect, lapply(yearly_ncai_matrices, colnames))

  if (is.null(habitats)) {
    habitats <- habitat_labels
  } else {
    habitats <- expand_groups(habitats, habitats_label_tree)
    check_labels(habitats, habitat_labels, "habitat", "habitats_label_tree")
  }

  if (is.null(services)) {
    services <- service_labels
  } else {
    services <- expand_groups(services, es_label_tree)
    check_labels(services, service_labels, "service", "es_label_tree")
  }

  filtered_matrix_list <- lapply(yearly_ncai_matrices, function(m) {
    m[habitats, services, drop = FALSE]
  })

  index_and_smooth(filtered_matrix_list,
                   smoothing_weights = smoothing_weights,
                   year_one = year_one)
}
