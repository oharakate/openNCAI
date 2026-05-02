#' Build All Natural Capital Asset Matrices
#'
#' Orchestrates the generation of asset matrices for every year in the series.
#' Each matrix represents the final "Year Sheet" in the NatureScot account
#' methodology, combining condition, wellbeing, and extent.
#'
#' @param tyf_list A named list of Total Yearly Flow (TYF) matrices (one per year).
#' @param wellbeing_potential_base A data frame representing the base wellbeing values
#'   (year one habitat extent weighted by provision per unit and demand).
#' @param habitat_extent A data frame where rows represent habitats and
#'   columns represent total extent per year.
#' @param year_one The base year (e.g., 2000) used for indexing extent.
#' @param year_list The list of years over which the index is calculated.
#' By default, \code{year_one} is taken as the first item in this list.
#' @param habitat_labels A character vector of habitat names to be applied
#'   to the resulting data frames.
#'
#' @return A named list of data frames, one for each year. Each data frame
#'   displays the calculated natural capital assets by habitat and service.
#' @keywords internal
build_all_ncai_matrices <- function(tyf_list,
                                    wellbeing_potential_base,
                                    habitat_extent,
                                    year_one = NULL,
                                    year_list,
                                    habitat_labels) {

  if (is.null(year_one)) {
    year_one = as.character(year_list[[1]])
  }

  # Iterate over the years provided in the tyf_list
  all_ncai <- lapply(names(tyf_list), function(yr, labels) {

    ncai_df <- build_ncai_matrix(
      tyf = tyf_list[[yr]],
      wellbeing_potential_base = wellbeing_potential_base,
      habitat_extent = habitat_extent,
      target_year = yr,
      year_one = year_one,
      habitat_labels = labels
    )

    # Ensure row names are explicitly set for clarity in output
    rownames(ncai_df) <- habitat_labels

    return(ncai_df)
  }, labels = habitat_labels)

  names(all_ncai) <- names(tyf_list)
  return(all_ncai)
}

#' Build a Single Year's Natural Capital Asset Matrix
#'
#' Calculates the Natural Capital Asset (NCA) matrix for a specific target year.
#' It combines the Total Yearly Flow (condition) with the Wellbeing Base and
#' adjusts for the change in Habitat Extent relative to a base year.
#'
#' @details
#' The final calculation follows the formula:
#' \eqn{(TYF \times Wellbeing Base \times Extent Index) / 10,000}.
#' The Extent Index is the target year's extent divided by the base year's
#' extent, multiplied by 100.
#'
#' @param tyf A matrix of Total Yearly Flows for the target year.
#' @param wellbeing_potential_base A matrix of base wellbeing values.
#' @param habitat_extent A data frame of habitat extent values.
#' @param target_year The specific year to calculate.
#' @param year_one The base year for extent indexing.
#'
#' @return A data frame of natural capital assets for the target year.
#' @keywords internal
build_ncai_matrix <- function(tyf,
                              wellbeing_potential_base,
                              habitat_extent,
                              target_year,
                              year_one,
                              habitat_labels) {

  # Get index habitat extent values for target year:
  extent_index <- get_habitat_extent_year_vec(target_year = target_year,
                                              year_one = year_one,
                                              habitat_extent = habitat_extent)

  # Step 1: Element-wise multiplication of condition (TYF) and wellbeing base (WB)
  wb_tyf <- as.matrix(tyf) * as.matrix(wellbeing_potential_base)

  # Step 2: Apply the extent index across the rows (Habitats)
  # sweep() applies the vector (extent_index) to each row of the matrix
  ncai_matrix <- sweep(
    x = wb_tyf,
    MARGIN = 1,
    STATS = extent_index,
    FUN = "*"
  )

  # Reapply rownames
  rownames(ncai_matrix) <- habitat_labels

  # Final Step: Normalize as per spreadsheet calculation (/10,000)
  return(as.data.frame(ncai_matrix / 10000))
}
