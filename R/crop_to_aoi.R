#' Title
#'
#' @param crs_matched_input A spatial object containing habitat extent data.
#' @param aoi_shape A spatial object containing area of interest boundary.
#' @param crop_fn A function used to perform the geometric cropping operation.
#'   It must be a function that accepts the object to be cropped (first argument)
#'   and the cropping boundary (second argument). The default is \code{\link[sf]{st_crop}},
#'   which uses the bounding box of \code{aoi_shape}. Users may supply custom
#'   functions for alternative clipping logic (e.g., \code{st_intersection}).
#' @param ...Additional arguments passed to the function specified by \code{crop_fn}.
#'   This allows users to customize the behavior of the default function, such as
#'   providing the \code{snap} argument to \code{\link[sf]{st_crop}}.
#'
#' @return A spatial object which holds the habitat extent data cropped to the
#'  are of interest.
#' @export
#'
#' @examples
#' # Habitat extent data (a large rectangle)
#' extent_data <- sf::st_polygon(list(
#'   matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
#' ))
#' habitat_sf <- sf::st_sfc(extent_data, crs = 4326)
#' habitat_sf <- sf::st_sf(id = 1, geom = habitat_sf)
#'
#' # Area of Interest (a smaller circle)
#' center_point <- sf::st_point(c(5, 5))
#' center_sfc <- sf::st_sfc(center_point, crs = 4326)
#' # Creating a circular AOI by buffering the point by 2 units
#' aoi_sf <- sf::st_buffer(center_sfc, dist = 2)
#' aoi_sf <- sf::st_sf(id = 1, geom = aoi_sf)
#'
#' # The habitat object is cropped to the bounding box of the AOI circle.
#' cropped_result <- crop_to_aoi(
#'   crs_matched_input = habitat_sf,
#'   aoi_shape = aoi_sf
#' )
#'
#' # Check the number of rows (should still be 1) and the geometry type
#' print(sf::st_geometry_type(cropped_result, by_feature = FALSE))
#'
#'
crop_to_aoi <- function(crs_matched_input,
                        aoi_shape,
                        crop_fn = sf::st_crop, ...) {

  cropped_object <- crop_fn(crs_matched_input, aoi_shape, ...)

  return(cropped_object)
}

