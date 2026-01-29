  ## BETA This script holds functions to trim a shapefile of data for mapping
  ## or analysing to the shape of an area of interest shapefile.
  ## Kate O'Hara
  ## 05-11-2025

# The main function in this script, crop_and_intersect(), takes two
# shapefile paths, that of the input data and that of the area-of-interest
# boundary. It harmonises the input data to the CRS of the AOI if necessary
# and then crops the input data to the shape of the AOI. It crops to a box
# first, and then to the detailed shape.

# The original script section used in cropping HabMoS to Forth2O AOI is pasted
# in comments below.

# One concern here is inconsistency in whether I declare sf_functions in the
# arguments, or just use them directly. I saw this done differently in
# different places and when I asked my LLM it gave inconsistent advice, so keen
# to know what Chris Littleboy thinks.

# MAKE SPATIAL OBJECTS
load_shape <- function(file_path) {

  if (!file.exists(file_path)) {
    stop(paste("File not found at path:", file_path))
  }

  spatial_object <-  sf::read_sf(file_path, quiet = TRUE)
  #  ^ quiet = TRUE is Gemini suggstion. I'm not sure if appropriate.
  return(spatial_object)
}

# CHECK CRS MATCH - should prob add fn to convert if not
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

# CROP TO AOI
crop_to_aoi <- function(crs_matched_input, aoi_shape,
                        crop_fn = sf::st_crop, ...) {

  cropped_object <- crop_fn(crs_matched_input, aoi_shape, ...)

  return(cropped_object)
}

# INTERSECT TO AOI
intersect_cropped <- function(cropped_object, aoi_shape) {

  intersected_object <- sf::st_intersect(cropped_object, aoi_shape)

  return(intersected_object)
}

# WRAPPER - STAGE THESE FUNCTIONS:
crop_and_intersect <- function(input_data_path, aoi_data_path) {

  # Collect input and aoi objects
  input_object <- load_shape(input_data_path)
  aoi_object <- load_shape(aoi_data_path)

  # Harmonise CRS's
  harmonised_input <- harmonise_crs(input_object, aoi_object)

  # Crop to box first (we think this will be faster than going straigh to
  # intersect?)
  cropped_to_box <- crop_to_aoi(harmonised_input, aoi_object)

  # Intersect fully
  aoi_shaped_data <- intersect_cropped(cropped_to_box, aoi_object)

  return(aoi_shaped_data)
}




#### ORIGINAL CODE PASTED FOR REFERENCE BELOW HERE ####
# #### CROP HABMOS TO AREA OF INTEREST ####
#
# ## Most recent area of interest file (aoi):
# aoi <- read_sf(file.path("data", "f2oaoi.shp"))
# head(aoi)
# st_crs(aoi)
# class(aoi)
# # It's in British National Grid projection.
# str(aoi)
# summary(aoi)
# names(aoi)
#
# ## HabMoS:
# eunis <- read_sf(file.path("data", "HABMOS_SCOTLAND.shp"))
# head(eunis)
# st_crs(eunis)
# # Also in British National Grid projection.
# str(eunis)
# summary(eunis)
# names(eunis)
#
#
# ## Crop EUNIS to AOI box and then exact shape:
# # Crop to box
# box_feunis <- st_crop(eunis, aoi)
#
# # For testing - check with plot:
# # ggplot(data = box_feunis) +
# #     geom_sf(fill = NA, color = "black") +
# #     theme_minimal() +
# #     labs(title = "Outline of box_eunis Polygons")
# # Looks right.
#
# st_write(box_feunis, file.path("data", "box_eunis.shp"), append=FALSE)
# # Note this is an extraneous save-out ^
#
# ## Crop to exact boundary
# intersect_feunis <- st_intersection(box_feunis, aoi)
#
# # Check with plot:
# ggplot(data = intersect_feunis) +
#   geom_sf(fill = "NA", color = "navyblue") +
#   theme_minimal() +
#   labs(title = "Outline of forth_eunis Polygons")
# # We have north Fife, tick.
#
# # Export:
# st_write(intersect_feunis, file.path("data", "habmos_trimmed.shp"), append=FALSE)


