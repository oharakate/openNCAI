# Original was:

  #' @param return Character. Specifies the object to return. Options include:
  #' \itemize{
  #'   \item \code{"index_overall"}: The standard, single-figure overall NCAI (default).
  #'   \item \code{"index_by_st"}: NCAI broken down by Ecosystem Service Type.
  #'   \item \code{"index_by_bh"}: NCAI broken down by Broad Habitat.
  #'   \item \code{"ncai_matrices"}: A list of matrices expressing natural
  #'   capital assets by habitat/ecosystem service.
  #'   \item \code{"provision_per_unit"}: The Ecosystem Service Potential Base matrix.
  #'   \item \code{"wellbeing_potential"}: The Wellbeing Base matrix.
  #'   \item \code{"flow_of_services"}: A list of yearly condition/flow
  #'   matrices.
  #'   \item \code{"everything"}: A named list containing all of the above.
  #' }
  #'
  #'
# Propose:
#' @param return Character. Specifies the object to return. Options include:
#' \itemize{
#'   \item \code{"ncai_overall"}: The standard, single-figure overall Natural
#'   Capital Assets Index (default).
#'   \item \code{"ncai_by_ecosystem_service_type"}: NCAI broken down by Ecosystem Service Type.
#'   \item \code{"ncai_by_broad_habitat"}: NCAI broken down by Broad Habitat.
#'   \item \code{"ncai_matrices"}: A list of matrices expressing natural
#'   capital assets by habitat/ecosystem service.
#'   \item \code{"esp_base"}: Ecosystem service potential
#'   base: the physical capacity of habitats in year one to provide ecosystem services.
#'   \item \code{"wellbeing_base"}: The demand-weighted contribution to
#'   population wellbeing, through ecosystem services, in year one.
#'   \item \code{"flow_of_services"}: Yearly estimated flow of ecosystem
#'   services based on condition of habitats.
#'   \item \code{"everything"}: A named list containing all of the above.
#' }
