#' Calculate the Ecosystem Service Potential Base
#'
#' Multiply the Provision Per Unit (weights) by the
#' habitat extent in the baseline year of the index.
#'
#' @param provision_per_unit_weights A data frame containing the ecosystem service provision
#' potential per unit weights. Row order must match the expanded habitats label
#' tree and column order must match the expanded ES label tree.
#' @param habitat_extent A data frame containing data representing the extent
#' (area) of each habitat in each year of the index. Rows = habitats. Columns =
#' years. There must be a column to match the identified baseline year. Row
#' order must match the expanded habitats label tree.
#' @param year_list A vector (character or numeric) of the years over which the
#' index is to be calculated.
#' @param year_one Optional. The specific year from \code{habitat_extent} to
#' use as the baseline for extent. If \code{NULL} (default), uses the first
#' element of \code{year_list}.
#' @param habitats_label_tree A named list of character vectors representing the
#' hierarchy of habitats.
#' @param es_label_tree A named list of character vectors representing
#' the hierarchy of ecosystem services.
#'
#' @return A labelled data frame with the same dimensions as 'provision_per_unit_weights'.
#' @keywords internal
#'
#' @examples
#' h_tree <- list(coastal = c("b1", "b2"), woodland = c("g1"))
#' es_tree <- list(provisioning = c("crops", "timber"))
#'
#' # Setup Habitat Extent
#' extent <- data.frame(
#'   `2026` = c(100, 150, 200),
#'   `2027` = c(110, 140, 210),
#'   check.names = FALSE,
#'   row.names = c("b1", "b2", "g1")
#' )
#'
#' # Setup Provision Per Unit Weights
#' weights <- data.frame(
#'   crops = c(0.12, 0.1, 0.0),
#'   timber = c(0.0, 0.0, 0.9),
#'   row.names = c("b1", "b2", "g1")
#' )
#'
#' years <- c("2026", "2027")
#'
#' es_potential_base_res <- openNCAI:::calc_es_potential_base(
#'   provision_per_unit_weights = weights,
#'   habitat_extent = extent,
#'   year_list = years,
#'   habitats_label_tree = h_tree,
#'   es_label_tree = es_tree)
calc_es_potential_base <- function(provision_per_unit_weights,
                      habitat_extent,
                      year_list,
                      year_one = NULL,
                      habitats_label_tree,
                      es_label_tree) {

  # 1. Validation checks (Unchanged)
  if (length(unlist(habitats_label_tree, use.names = FALSE)) != nrow(habitat_extent)) {
    stop("Number of habitat names in habitats_label_tree must match rows in habitat_extent.")
  }

  if (!all(rownames(habitat_extent) == rownames(provision_per_unit_weights))) {
    stop("Row names (habitats) of habitat_extent and provision_per_unit_weights must be identical.")
  }

  # 2. Identify target year
  target_year <- as.character(if (is.null(year_one)) year_list[1] else year_one)

  if (!(target_year %in% colnames(habitat_extent))) {
    stop(paste0("Baseline year '", target_year, "' not found in habitat_extent columns."))
  }

  # 3. Pull the vector for the identified baseline year
  origin_year_vec <- dplyr::pull(habitat_extent, var = target_year)

  # 4. Multiply habitat extent values across the provision_per_unit weightings
  es_potential_base <- provision_per_unit_weights * origin_year_vec

  rownames(es_potential_base) <- rownames(provision_per_unit_weights)

  # 5. Label and return
  return(label_ncai_matrix(es_potential_base, habitats_label_tree, es_label_tree))
}
