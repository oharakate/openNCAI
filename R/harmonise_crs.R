## HARMONISE THE CRS OF THE INPUT DATA TO MATCH THAT OF THE AREA OF INTEREST
## SHAPEFILE

#' Title
#'
#' @param input_object A shapefile containing habitats data
#' @param aoi_object A shapefile delineating the extent of the area of interest
#'
#' @return The input object with CRS matched to the AOI.
#' @export
#'
#' @examples
#'
#' wrong_crs_input <- sf::st_read(
#'   system.file("extdata", "wrong_crs_data.shp", package = "openNCAI"),
#'     quiet = TRUE)
#' aoi <- sf::st_read(
#'   system.file("extdata", "test_aoi.shp", package = "openNCAI"),
#'     quiet = TRUE)
#'
#' crs_harmonised_output <- harmonise_crs(wrong_crs_input, aoi)
#'
#' sf::st_crs(wrong_crs_input) # The original (mismatched) CRS
#' sf::st_crs(aoi)        # The target AOI CRS
#' sf::st_crs(crs_harmonised_output) # The output CRS (should match the AOI)
#'
#'
harmonise_crs <- function(input_object, aoi_object) {

  if (sf::st_crs(input_object) != sf::st_crs(aoi_object)) {
    message("CRS mismatch detected. Transforming input data to match CRS of AOI.")
    crs_matched_input <- sf::st_transform(input_object, sf::st_crs(aoi_object))
  }
  else {
    crs_matched_input <- input_object
  }

  return(crs_matched_input)
}


