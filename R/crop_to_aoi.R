#' Title
#'
#' @param crs_matched_input A spatial object containing habitat extent data.
#' @param aoi_shape A spatial object containing area of interest boundary.
#' @param crop_fn A function used to perform the geometric cropping operation.
#'   It must be a function that accepts the object to be cropped (first argument)
#'   and the cropping boundary (second argument). The default is \code{\link[sf]{st_crop}},
#'   which uses the bounding box of \code{aoi_shape}. Users may supply custom
#'   functions for alternative clipping logic (e.g., \code{st_intersection}).
#' @param ... Additional arguments passed to the function specified by \code{crop_fn}.
#'   This allows users to customize the behavior of the default function, such as
#'   providing the \code{snap} argument to \code{\link[sf]{st_crop}}.
#'
#' @return A spatial object which holds the habitat extent data cropped to the
#'  are of interest.
#' @export
#'
#' @examples
#' # Use a Projected CRS (EPSG: 27700 = UK OS National Grid, units are meters)
#'
#' # 1. Habitat extent data (a 20km x 20km area)
#' easting_min <- 400000; easting_max <- 420000
#' northing_min <- 300000; northing_max <- 320000
#'
#' extent_data <- sf::st_polygon(list(
#'   matrix(c(easting_min, northing_min, easting_max, northing_min,
#'            easting_max, northing_max, easting_min, northing_max,
#'            easting_min, northing_min), ncol = 2, byrow = TRUE)
#' ))
#' habitat_sfc <- sf::st_sfc(extent_data, crs = 27700)
#' habitat_sf <- sf::st_sf(id = 1, geom = habitat_sfc)
#'
#' # 2. Area of Interest (a 5 km radius circle)
#' # Center point (in meters)
#' center_point <- sf::st_point(c(410000, 310000))
#' center_sfc <- sf::st_sfc(center_point, crs = 27700)
#'
#' # Creating a circular AOI by buffering the point by 5000 meters (5 km)
#' aoi_sf <- sf::st_buffer(center_sfc, dist = 5000)
#' aoi_sf <- sf::st_sf(id = 1, geom = aoi_sf)
#'
#' # 3. Run the Function
#' # The habitat object is cropped to the bounding box of the AOI circle.
#' cropped_result <- crop_to_aoi(
#'   crs_matched_input = habitat_sf,
#'   aoi_shape = aoi_sf
#' )
#'
#' # Check the resulting geometry type
#' print(sf::st_geometry_type(cropped_result))
#'
#'
crop_to_aoi <- function(crs_matched_input,
                        aoi_shape,
                        crop_fn = sf::st_crop, ...) {

  cropped_object <- crop_fn(crs_matched_input, aoi_shape, ...)

  return(cropped_object)
}

