#' Intersect Spatial Objects with an Area of Interest
#'
#' This function performs a spatial intersection between a spatial
#' object and a defined Area of Interest (AOI) shape, trimming the spatial
#' object to the exact shape of the AOI.
#'
#' @param cropped_object An 'sf' object representing the layer to be
#' intersected.
#' @param aoi_shape An 'sf' object representing the shape to intersect to.
#'
#' @return An 'sf' object containing only the portions of 'cropped_object' that
#' overlap with 'aoi_shape'.
#' @export
#'
#' @examples
#' library(sf)
#' #
#'
intersect_cropped <- function(cropped_object, aoi_shape) {

  intersected_object <- sf::st_intersect(cropped_object, aoi_shape)

  return(intersected_object)
}
