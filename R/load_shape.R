

# MAKE SPATIAL OBJECTS FROM FILE PATHS
#' Make a spatial object from a shapefile
#'
#' @description
#' Takes the file path of a shape file, reads in using sf, and returns a
#' spatial object.
#'
#' @param file_path A file path string.
#'
#' @return A spatial object.
#' @export
#'
#' @examples
#' shapefile_path <- system.file(
#' "extdata","test_data.shp", package = "openNCAI")
#' load_shape(shapefile_path)
#'
load_shape <- function(file_path) {

  if (!file.exists(file_path)) {
    stop(paste("File not found at path:", file_path))
  }

  spatial_object <-  sf::read_sf(file_path, quiet = TRUE)
  return(spatial_object)
}
