  ## BETA This script holds functions to render cleaned and categorised
  ## habitat data for display on the Explore map, including raster functions.
  ## Kate O'Hara 
  ## 05-11-2025
  

# Classification as mosaic has been moved to the classification function. 
# Also the catch for "NONE" on majhabs. 
# Check that the wrapper function checks existence of /rasters/ dir.



# FN: ARCHIVE COPIES OF EXISTING RASTERS
# A good amount of help from Gemini in this one!
# This function makes and stores a timestamped copy of the old rasters before
# overwriting, such that access to the raster file is not lost in the event
# of failure of the processing routine. 
archive_old_output <- function(base_filename, base_dir = "rasters") {
  
  archive_dir <- file.path(base_dir, "archive")
  final_output_path <- file.path(base_dir, base_filename)
  
  # Ensure the archive directory exists silently
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check if the fixed-name output file already exists
  if (file.exists(final_output_path)) {
    
    # Create a unique timestamped name for the archive file
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    archived_filename <- paste0(
      tools::file_path_sans_ext(base_filename), # Filename without extension
      "_", timestamp, 
      ".", tools::file_ext(base_filename)      # Original extension
    )
    archived_path <- file.path(archive_dir, archived_filename)
    
    message(paste("Archiving existing file via copy to:", archived_path))
    
    # Copy (duplicate) the file to the archive folder
    success <- file.copy(from = final_output_path, to = archived_path)
    
    if (success) {
      message("Archiving successful. Original file remains, ready to be overwritten.")
    } else {
      # If copy fails, issue a warning but don't stop the code
      warning("ERROR: Failed to copy the old file to the archive. Proceeding without archiving.")
    }
    
  } else {
    message(paste("No old file found at", final_output_path, "—Proceeding."))
  }
  
  invisible(NULL)
}


# FN: CLEAN BEFORE MAPPING:
# This function selects vars and does last cleaning before mapping. 
clean_for_mapping <- function(shape_object) {
  
  clean_data <- shape_object %>% 
    # Sort by pgn_id
    dplyr::arrange(pgn_id) %>%
    # Keep only vars for rastering layers
    dplyr::select(
      c("pgn_id", "source", "dethabs", "hab_prop", "brodhab", "brodnum", 
        "majhabs", "eunis1", "eu1num", "eu1lab", "eu1let",
        "is_eunis1_mosaic", "is_ncai_mosaic")) %>% 
    # Some habitat proportions are just over 1; replace with 1:
    dplyr::mutate(hab_prop = pmin(hab_prop, 1))
  
  return(clean_data)
}



# FN: APPLY LAYER ORDERING PREFERENCES
# This function creates a variable which records the prioritisation of 
# different surveys to show the maximum useful information in the single-
# layer map image. 
# Note that Montane dataset is going to need added here. 
set_layer_order <- function(shape_object) {
  
  data_with_layer_order <- shape_object %>% 
    dplyr::mutate(
      prefsrc = dplyr::case_when(
        stringr::str_detect(source, "Coastal") == TRUE ~ 1,
        stringr::str_detect(source, "Saltmarsh") == TRUE ~ 2,
        stringr::str_detect(source, "Dune") == TRUE ~ 3,
        stringr::str_detect(source, "SIACS") == TRUE ~ 4,
        stringr::str_detect(source, "NWSS") == TRUE ~ 5,
        stringr::str_detect(source, "Forest") == TRUE ~ 6,
        stringr::str_detect(source, "Freshwater") == TRUE ~ 7,
        stringr::str_detect(source, "NVC") == TRUE ~ 8,
        stringr::str_detect(source, "Landuse") == TRUE ~ 9,
        TRUE ~ NA_integer_
      )
    ) 
  # ^ As per Chris's original preferred order:
  # 1) Coastal Vegetated Shingle, 
  # 2) Saltmash Survey 
  # 3) Sand Dune Vegetation Survey, 
  # 4) SIACS, 
  # 5) NWSS
  # 6) National Forest Inventory 
  # 7) Freshwater, 
  # 8) NVC Conversion 
  # 9) Historic Landuse Assessment
  # View(feunis_brodhabs)
  
  return(data_with_layer_order)
}


## FN: MAKE A TEMPLATE RASTER
# This function takes the trimmed, cleaned, classified shapefile and makes a
# template raster from it.
make_template_raster <- function(shape_object) {
  # Set extent
  template_extent <- sf::st_bbox(shape_object)
  # Set crs
  template_crs <- sf::st_crs(shape_object)$wkt # Use the CRS from the passed 
                                               # shapefile in wkt
  # Set resolution
  desired_resolution <- 10
  
  # Make raster
  template_raster <- terra::rast(
    extent = template_extent,
    resolution = desired_resolution,
    crs = template_crs 
  )
  
  return(template_raster)
}

## FN: MAKE COVERAGE RASTER
# This function takes the trimmed, cleaned and classified shapefile and 
# rasterizes it as a single layer showing coverage (of clean HabMoS) data as
# a binary value. It writes a geoTIFF (archives the old one) and returns
# the raster object. 
# Is there any need to be able to change the save location here?
rasterize_coverage <- function(shape_object,  template_raster) {
  
  # Check if template is a SpatRaster object and not the default string
  # (This is a Gemini recommendation)
  if (!inherits(template_raster, "SpatRaster")) {
    stop("template_raster must be a valid SpatRaster object from the terra package.")
  }
  
  # Use only polygons with valid EUNIS1 habitat data
  data_to_rasterize <- shape_object %>% 
    dplyr::filter(!is.na(eunis1))%>%
  
  # Mark all rows as covered:
    dplyr::mutate(covered = 1)
  
  # rasterize
  raster_coverage <-  terra::rasterize(
    x = data_to_rasterize, 
    y = template_raster, 
    field = "covered",
    fun = max)
  
  # Archive the old file before overwriting (use archive_old_output() above)
  archive_old_output(base_filename = "coverage_overlay.tif")
  
  # Make GeoTIFF
  terra::writeRaster(
    raster_coverage,
    file.path("rasters", "coverage_overlay.tif"),
    overwrite = TRUE
  )
  
  # Return the raster JIC.
  return(raster_coverage)
}



## FN: MAKE EUNIS1 RASTER
# This function takes the trimmed, cleaned and classified shapefile and 
# rasterizes it as a single layer showing EUNIS level 1 classified areas as a 
# single layer, prioritising surveys as per set_layer_order(). EUNIS1 mosaics
# are shown as "Mixed".
# It writes a geoTIFF (archives the old one) and returns
# the raster object. 
rasterize_eunis1 <- function(shape_object, template_raster) {
  
  # Ensure template_raster is the correct object class
  if (!inherits(template_raster, "SpatRaster")) {
    stop("template_raster must be a valid SpatRaster object from the terra package.")
  }
  
  # Filter to only valid EUNIS1 data, and make sure mosaics are coded Mixed
  data_to_rasterize <- shape_object %>% 
    dplyr::filter(!is.na(eunis1)) %>% 
    dplyr::mutate(eu1lab = dplyr::if_else(is_eunis1_mosaic, "Mixed", eu1lab)) %>% 
    dplyr::mutate(eu1num = dplyr::if_else(eu1lab=="Mixed", 11, eu1num))
  
  # Pivot wider on eu1lab, use prefsrc value to prepare for layer ordering
  data_wider <- data_to_rasterize %>% 
    dplyr::filter(!is.na(eu1lab)) %>%
    dplyr::distinct(pgn_id, eu1lab, .keep_all = TRUE) %>%
    tidyr::pivot_wider(
      id_cols = c(pgn_id, prefsrc, geometry),
      names_from = eu1lab,
      values_from = eu1num,
      values_fill = 0
    )  %>%
    dplyr::mutate(layer =
             Built +
             Coastal +
             Cropland +
             Freshwater +
             Grassland +
             Heathland +
             Marine +
             Unvegetated +
             Wetland + 
             Woodland +
             Mixed +
             (100 * prefsrc)
    ) %>%
    dplyr::filter(layer!=0)
  
  # Rasterise choosing min value (to prioritise layers)
  raster_eunis1_onelayer <- terra::rasterize(data_wider,
                                      template_raster,
                                      field = "layer",
                                      fun = "min")
  
  # Use modulo to get back to the eunis1 number value
  raster_eunis1_onelayer <- raster_eunis1_onelayer %% 100
  
  # Archive the old file before overwriting (use archive_old_output() above)
  archive_old_output(base_filename = "eunis1_categorical_overlay.tif")
  
  # Save the raster as GeoTIFF:
  terra::writeRaster(
    raster_eunis1_onelayer,
    file.path("rasters", "eunis1_categorical_overlay.tif"),
    overwrite = TRUE)

  return(raster_eunis1_onelayer)
  
}



## FN: MAKE NCAI RASTER
# # This function takes the trimmed, cleaned and classified shapefile and 
# rasterizes it as a single layer showing NCAI broad habitats - just the seven
# which are typically highlighted in NatureScot reports - as a 
# single layer, prioritising surveys as per our set_layer_order(). EUNIS1 
# mosaics are shown as "Mixed".
# It writes a geoTIFF (archives the old one) and returns
# the raster object. 
rasterize_ncai <- function(shape_object, template_raster) {
  
  # Ensure template_raster is the correct object class
  if (!inherits(template_raster, "SpatRaster")) {
    stop("template_raster must be a valid SpatRaster object from the terra package.")
  }
  
  # Filter to only valid ncai_broad data, and mark mosaics as Mixed:
  data_to_rasterize <- shape_object %>% 
    dplyr::filter(!is.na(brodhab)) %>% 
    dplyr::mutate(brodhab = dplyr::if_else(is_ncai_mosaic, "Mixed", brodhab)) %>% 
    dplyr::mutate(brodnum = dplyr::if_else(brodhab=="Mixed", 8, brodnum))
  
  # Pivot wider and create a factor variable to use in the raster:
  data_wider <- data_to_rasterize %>%
    dplyr::filter(!is.na(brodhab)) %>%
    
    # Use distinct to deal with multiples of brodhab within one
    # polygon:
    dplyr::distinct(pgn_id, brodhab, .keep_all = TRUE) %>%
    # Pivot wider
    tidyr::pivot_wider(
      id_cols = c(pgn_id, prefsrc, geometry),
      names_from = brodhab,
      values_from = brodnum,
      values_fill = 0
    )  %>%
    # Use prefsrc to add layer order:
    dplyr::mutate(layer =
             Mixed +
             Coastal +
             Freshwater +
             Wetland +
             Grassland +
             Heathland +
             Woodland +
             Cropland + 
             (100 * prefsrc)
    ) %>%
    dplyr::filter(layer!=0)
  
  # Rasterize this, selecting min to apply order:
  raster_ncai_onelayer <- terra::rasterize(data_wider,
                                    template_raster,
                                    field = "layer", 
                                    fun = "min")
  
  # Use modulo to return to category value 
  # Note use of %%100 this time because fewer than 10 categories here - WATCH.
  raster_ncai_onelayer <- raster_ncai_onelayer %% 100
  
  # Save the raster as GeoTIFF:
  terra::writeRaster(
    raster_ncai_onelayer,
    "rasters/ncai_categorical_overlay.tif",
    overwrite = TRUE
  )
  
  # Return the raster object JIC
  return(raster_ncai_onelayer)
  
}


## FN: WRAPPER - MAKE MAP LAYERS FROM CLEAN, TRIMMED SHAPEFILES 
# Note that this function passes the same clean input data back out again to 
# allow the NCAI calculation to take place. 
# I've learned this is called a Tee function and we could use %T% in the master
# script to achieve this actually. 
make_map_layers <- function(
    clean_trimmed_data, 
    coverage = TRUE, 
    eunis1 = TRUE,
    ncai = TRUE) {
  
  # Final clean for rasterizing
  data_for_processing <- clean_trimmed_data %>% 
    clean_for_mapping() %>% 
    set_layer_order() %>% 
    label_classifications()
  
  # Generate Template Raster
  template_raster <- make_template_raster(data_for_processing)
  
  # Define the fixed output filenames for each classification scheme
  output_filenames <- list(
    coverage = "coverage_overlay.tif",
    eunis1   = "eunis1_categorical_overlay.tif", 
    ncai     = "ncai_categorical_overlay.tif"
  )
  
  if (coverage) {
    archive_old_output(output_filenames$coverage)
    rasterize_coverage(data_for_processing, template_raster)
  }
  
  if (eunis1) {
    archive_old_output(output_filenames$eunis1)
    rasterize_eunis1(data_for_processing, template_raster)
  }
  
  if (ncai) {
    archive_old_output(output_filenames$ncai)
    rasterize_ncai(data_for_processing, template_raster)  
    }
  
  # Returns same clean data that was passed, for processing differently
  return(clean_trimmed_data)
}


#### ORIGINAL CODE PASTED FOR REFERENCE BELOW HERE ####
#### PREPARE FOR MAPPING ####

# Little bit of cleanup and get the list of categories:

# feunis_brodhabs <- st_read(file.path("data", "feunis_brodhabs.shp")) %>%
#   arrange(pgn_id) %>% 
#   select(c("pgn_id", "source", "dethabs", "hab_prop", "brodhab", "brodnum", "majhabs", "eunis1", "eu1num", "eu1lab", "eu1let", "mosaic" ))



# Add a variable which denotes the preferred order in which to use datasets
# from different sources when choosing which cell to use in a single layer 
# raster, if the cell is touched by polygons from more than one source.
# feunis_brodhabs <- feunis_brodhabs %>% 
#   mutate(
#     prefsrc = case_when(
#       str_detect(source, "Coastal") == TRUE ~ 1,
#       str_detect(source, "Saltmarsh") == TRUE ~ 2,
#       str_detect(source, "Dune") == TRUE ~ 3,
#       str_detect(source, "SIACS") == TRUE ~ 4,
#       str_detect(source, "NWSS") == TRUE ~ 5,
#       str_detect(source, "Forest") == TRUE ~ 6,
#       str_detect(source, "Freshwater") == TRUE ~ 7,
#       str_detect(source, "NVC") == TRUE ~ 8,
#       str_detect(source, "Landuse") == TRUE ~ 9
#     )
#   ) 
# ^ As per Chris's preferred order:
# 1) Coastal Vegetated Shingle, 
# 2) Saltmash Survey 
# 3) Sand Dune Vegetation Survey, 
# 4) SIACS, 
# 5) NWSS
# 6) National Forest Inventory 
# 7) Freshwater, 
# 8) NVC Conversion 
# 9) Historic Landuse Assessment
# View(feunis_brodhabs)

# Only use those which have major eunis habitat data
# print(unique(feunis_brodhabs$eunis1))
# feunis_brodhabs <- feunis_brodhabs %>% 
  # filter(!is.na(eunis1))
# View(feunis_brodhabs)

# Get the number of distinct NCAI broad habitats and mark as ncai_mosaic if
# it's more than one:
# feunis_brodhabs <-  feunis_brodhabs %>% # moved to classification script.
#   group_by(pgn_id) %>% 
#   mutate(is_ncai_mosaic = (n_distinct(brodhab, na.rm = TRUE) > 1)) %>% 
#   ungroup()
# table(feunis_brodhabs$is_ncai_mosaic)
# Note that some which have eunis1 will not have ncai brodhab.
# So 4 lines above na.rm ignores those when counting. 

# See these mosaics
# ncai_mosaics <-  feunis_brodhabs %>% 
#   filter(is_ncai_mosaic)
# View(ncai_mosaics)
# table(ncai_mosaics$brodhab, useNA = "ifany")
# Looks right so can use that variable later. 
# MAKE SURE TO EXCLUDE BRODHAB==NA.
# remove(ncai_mosaics)


# Do the same for EUNIS 1. 
# feunis_brodhabs <-  feunis_brodhabs %>% # moved to classification script.
#   group_by(pgn_id) %>% 
#   mutate(is_eunis1_mosaic = (n_distinct(majhabs, na.rm = TRUE) > 1)) %>% 
#   ungroup()
# table(feunis_brodhabs$is_eunis1_mosaic)
# eunis1_mosaics <-  feunis_brodhabs %>% 
#   filter(is_eunis1_mosaic)
# # View(eunis1_mosaics)
# table(eunis1_mosaics$eunis1, useNA = "ifany")
# remove(eunis1_mosaics)


# Some habitat proportions are (very slightly) higher than one; set them to 1.
# summary(feunis_brodhabs$hab_prop)
# overone <- feunis_brodhabs %>% 
#   filter(hab_prop > 1)
# table(overone$hab_prop)
# feunis_brodhabs <- feunis_brodhabs %>%
#   mutate(hab_prop = pmin(hab_prop, 1))
# overone <- feunis_brodhabs %>% 
#   filter(hab_prop > 1)
# table(overone$hab_prop)
# remove(overone)


# MAKE A TEMPLATE RASTER :
# Creating a template raster
# We are going for 10m res.
# template_extent <- ext(feunis_brodhabs)
# template_extent
# desired_resolution <- 10
# 
# template_raster <- rast(
#   extent = template_extent,
#   resolution = desired_resolution,
#   crs = crs(feunis_brodhabs) # Use the CRS from the original shapefile
# )
# print(template_raster)

# Make a folder for GeoTIFFs to go to the map view:
# dir.create("rasters", showWarnings =FALSE)

####


#### DATA COVERAGE LAYER  #####
# Feunis coverage only:
# feunis_coverage <- feunis_brodhabs %>%
#   # Get rid of uncategorised and missing
#   filter(
#     !is.na(eunis1)
#   ) %>%
#   
#   filter(
#     
#   ) %>%
#   # Mark all rows as covered:
#   mutate(covered = 1)
# # Check:
# table(feunis_coverage$eunis1, useNA = "ifany")
# table(feunis_coverage$covered)
# 
# #
# # # rasterize and make tiff:
# raster_coverage <-  rasterize(feunis_coverage, template_raster, field = "covered")
# print(raster_coverage)
# # plot(raster_coverage)
# 
# ggplot() +
#   # Map alpha to the value (1) and set a constant fill
#   geom_spatraster(
#     data = raster_coverage,
#     aes(alpha = after_stat(value)),
#     fill = "#800a01"
#   ) +
#   
#   # Explicitly control the transparency scale
#   scale_alpha_continuous(
#     na.value = 0,         # Makes NA cells fully transparent
#     range = c(0.7, 0.7),  # Makes all data cells 80% transparent (0.2 opacity)
#     guide = "none"        # Hides the legend for alpha
#   ) +
#   
#   coord_sf(crs = 27700) +
#   theme_minimal() +
#   labs(title = "Raster Coverage")
# 
# 
# # # Make GeoTIFF
# writeRaster(
#   raster_coverage,
#   "rasters/coverage_overlay.tif",
#   overwrite = TRUE
# )
# EXECUTED 05 NOV
####



#### NCAI 7 BROAD HABITATS LAYER ####

# We can see which polygons are parts of ncai mosaics:
# table(feunis_brodhabs$is_ncai_mosaic)

# We are going to call them mixed
# ncai_brodhabs <- feunis_brodhabs %>% 
#   filter(!is.na(brodhab)) %>% 
#   mutate(brodhab = if_else(is_ncai_mosaic, "Mixed", brodhab)) %>% 
#   mutate(brodnum = if_else(brodhab=="Mixed", 8, brodnum))
# Check so far:
# View(ncai_brodhabs)

# Column prefsrc holds the source sort order

# Pivot wider and create a factor variable to use in the raster:
# ncai_brodhabs_wider <- ncai_brodhabs %>%
#   filter(!is.na(brodhab)) %>%
#   
#   # Use distinct to deal with multiples of brodhab within one
#   # polygon:
#   distinct(pgn_id, brodhab, .keep_all = TRUE) %>%
#   #
#   pivot_wider(
#     id_cols = c(pgn_id, prefsrc, geometry),
#     names_from = brodhab,
#     values_from = brodnum,
#     values_fill = 0
#   )  %>%
#   mutate(layer =
#            Mixed +
#            Coastal +
#            Freshwater +
#            Wetland +
#            Grassland +
#            Heathland +
#            Woodland +
#            Cropland + 
#            (100 * prefsrc)
#   ) %>%
#   filter(layer!=0)

# Check the layer values - should not exceed 908
# print(unique(ncai_brodhabs_wider$layer))
# 
# # Rasterize this, selecting min (no need to repeat):
# raster_ncai_onelayer <- rasterize(ncai_brodhabs_wider,
#                                   template_raster,
#                                   field = "layer", 
#                                   fun = "min")
# 
# # AJUST RASTER TO JUST THE CATEGORY USING MODULO
# raster_ncai_onelayer <- raster_ncai_onelayer %% 10
# print(raster_ncai_onelayer)
# 
# # Save the raster as GeoTIFF:
# writeRaster(
#   raster_ncai_onelayer,
#   "rasters/ncai_categorical_overlay.tif",
#   overwrite = TRUE
# )

# raster_ncai_onelayer <- rast("rasters/ncai_categorical_overlay.tif")
# print(raster_ncai_onelayer)
# levels(raster_ncai_onelayer) <- category_map
# plot(raster_ncai_onelayer)

# EXECUTED 05 NOV



## An aside - a colormap for ncai 
# 
# # Lay out a colour map first:
# category_map_ncai <- tribble(
#   ~value, ~label,          ~color,
#   1, "Coastal",      "#53CCD0",
#   2, "Freshwater",   "#396CC4",
#   3, "Wetland",      "#F5A017",
#   4, "Grassland",    "#93BB18",
#   5, "Heathland",    "#933BA0",
#   6, "Woodland",     "#1B7942",
#   7, "Cropland",     "#C26E32",
#   8, "Mixed",        "#B69C5A"
# )
# 
# # tmap option:
# tmap_mode("view")
# 
# tm_shape(raster_ncai_onelayer) +
#   tm_raster(
#     col_alpha = 0.7,
#     col.scale = tm_scale_categorical(
#       values = category_map_ncai$color,
#       labels = category_map_ncai$label
#     ),
#     col.legend = tm_legend(
#       title = "NCAI Habitats"
#     )
#   ) +
#   tm_layout(legend.outside = TRUE)

# Inserting some code to export a colormap for QGIS:
# Use the category_map object we made above:
# Convert hex colors to an R, G, B matrix
# rgb_matrix_ncai <- t(col2rgb(category_map_ncai$color))

# Create the final data frame in the format QGIS needs
# qgis_colormap_ncai <- tibble(
#   value = category_map_ncai$value,
#   r = rgb_matrix_ncai[, "red"],
#   g = rgb_matrix_ncai[, "green"],
#   b = rgb_matrix_ncai[, "blue"],
#   alpha = 178, # Alpha channel (255 is fully opaque)
#   label = category_map_ncai$label
# )

# 4. Write this to a text file. A .txt file with commas works well.
# write_csv(qgis_colormap_ncai, "qgis_colormap_ncai.txt")

# But I never found a way to bring that into QGIS.
# 



#### EUNIS1 HABITATS RASTER ####

# eunis1_brodhabs <- feunis_brodhabs %>% 
#   filter(!is.na(eunis1)) %>% 
#   mutate(eu1lab = if_else(is_eunis1_mosaic, "Mixed", eu1lab)) %>% 
#   mutate(eu1num = if_else(eu1lab=="Mixed", 11, eu1num))
# # Check so far:
# # View(eunis1_brodhabs)
# # Looks right. 
# 
# # Pivot wider and create a factor variable to use in the raster:
# eunis1_brodhabs_wider <- eunis1_brodhabs %>%
#   filter(!is.na(eu1lab)) %>%
#   
#   # Use distinct to deal with multiples of brodhab within one
#   # polygon:
#   distinct(pgn_id, eu1lab, .keep_all = TRUE) %>%
#   #
#   pivot_wider(
#     id_cols = c(pgn_id, prefsrc, geometry),
#     names_from = eu1lab,
#     values_from = eu1num,
#     values_fill = 0
#   )  %>%
#   mutate(layer =
#            Built +
#            Coastal +
#            Cropland +
#            Freshwater +
#            Grassland +
#            Heathland +
#            Marine +
#            Unvegetated +
#            Wetland + 
#            Woodland +
#            Mixed +
#            (100 * prefsrc)
#   ) %>%
#   filter(layer!=0)
# 
# # Check the layer values - should not exceed 911
# print(unique(eunis1_brodhabs_wider$layer))
# # good
# 
# 
# # ADJUST TO CHOOSE MIN
# raster_eunis1_onelayer <- rasterize(eunis1_brodhabs_wider,
#                                     template_raster,
#                                     field = "layer",
#                                     fun = "min")
# # Use modulo to get back to category only
# raster_eunis1_onelayer <-  raster_eunis1_onelayer %% 100
# print(raster_eunis1_onelayer)
# 
# 
# # Save the raster as GeoTIFF:
# writeRaster(
#   raster_eunis1_onelayer,
#   "rasters/eunis1_categorical_overlay.tif",
#   overwrite = TRUE)
# # EXECUTED 05 NOV

# plot(raster_eunis1_onelayer)

# raster_eunis1_onelayer <- rast("rasters/eunis1_categorical_overlay.tif")


# category_map_eunis1 <- tribble(
#   ~value, ~label,          ~color,
# 
#   1, "Marine",      "#0C155B",
#   2, "Coastal",      "#53CCD0",
#   3, "Freshwater",   "#396CC4",
#   4, "Wetland",      "#F5A017",
#   5, "Grassland",    "#93BB18",
#   6, "Heathland",    "#933BA0",
#   7, "Woodland",     "#1B7942",
#   8, "Unvegetated",    "#846B79",
#   9, "Cropland",      "#C26E32",
#   10, "Built",        "#CD244E",
#   11, "Mixed",        "#B69C5A"
# )


# # tmap option:
# tmap_mode("view")
# 
# tm_shape(raster_eunis1_onelayer) +
#   tm_raster(
#     col_alpha = 0.7,
#     col.scale = tm_scale_categorical(
#       values = category_map_eunis1$color,
#       labels = category_map_eunis1$label
#     ),
#     col.legend = tm_legend(
#       title = "EUNIS 1 Habitats"
#     )
#   ) +
#   tm_layout(legend.outside = TRUE)



# Inserting some code to export a colormap for QGIS:
# Use the category_map object we made above:
# Convert hex colors to an R, G, B matrix
# rgb_matrix_eunis1 <- t(col2rgb(category_map_eunis1$color))

# Create the final data frame in the format QGIS needs
# qgis_colormap_eunis1 <- tibble(
#   value = category_map_eunis1$value,
#   r = rgb_matrix_eunis1[, "red"],
#   g = rgb_matrix_eunis1[, "green"],
#   b = rgb_matrix_eunis1[, "blue"],
#   alpha = 178, # Alpha channel (255 is fully opaque)
#   label = category_map_eunis1$label
# )

# write_csv(qgis_colormap_eunis1, "qgis_colormap_eunis1.txt")

####