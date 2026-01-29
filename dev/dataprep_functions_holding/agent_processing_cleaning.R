  ## BETA This script holds functions to clean trimmed habitat data, including
  ## trimming column names to 7 characters and adding polygon unique IDs
  ## Kate O'Hara 
  ## 05-11-2025

# So, cropping_to_aoi.R will return a spatial object which is the input data
# file trimmed to the size of an area of interest shapefile. 
# Main input here is a cropped_shapefile out of crop_and_intersect() in 
# cropping_to_aoi.R
# Main function here is cleanup_trimmed_data() which returns a shapefile object.

# Note that this takes a good few minutes to run. 

# FN: CLEAN UP COLNAMES, including bespoke renaming and cropping to 7 chars:
clean_colnames <- function(cropped_shp_object) {
  # This ^ could take an argument for a different set of variable names from a 
  # different year or something, but for now let's hard code for HabMoS as we 
  # used it. 
  
  # Column names to lower case:
  colnames(cropped_shp_object) <- tolower(colnames(cropped_shp_object))
  
  # Rename columns in use to max 7 characters to dodge potential renaming
  # behaviour when saving shapefiles.
  clean_shp_object <- cropped_shp_object %>%
    dplyr::rename(majhabs = symbol) %>%
    dplyr::rename(dethabs = habitat_co) %>%
    dplyr::rename(hab_nam = habitat_na) %>%
    dplyr::rename(source = habmos_sou) %>%
    dplyr::rename(svy_id = survey_id) %>%
    dplyr::rename(svy_dat = survey_dat) %>%
    dplyr::rename(svy_typ = survey_typ)
  
  # Any remaining names to 7 character max
  # colnames(clean_shp_object) <- substr(colnames(clean_shp_object), 1, 7)
  # Can't use that ^ because it breaks the geometry column's name.
  
  # Instead:
  # Get the name of the geometry column (should be "geometry")
  geom_col_name <- attr(clean_shp_object, "sf_column")
  current_names <- colnames(clean_shp_object)

  # sapply function to dodge geometry col and shorten other names
  # Note outputs list of names. 
  new_names <- sapply(current_names, FUN = function(col_name) {
    if (col_name == geom_col_name) {
      return(col_name) 
    } else {
      return(substr(col_name, 1, 7)) 
    }
  })
  
  # Apply the new names
  colnames(clean_shp_object) <- new_names  
  
  # Catch non-informative data in EUNIS1 habs and mark NA:
  clean_shp_object <- clean_shp_object %>% 
    dplyr::mutate(majhabs = dplyr::if_else(
      grepl("NONE", majhabs, ignore.case = FALSE) | 
        majhabs == "-" |                            
        majhabs == "No information",                 
      
      NA_character_, # Result if TRUE
      majhabs        # Result if FALSE
    ))

  # For testing:
  # table(clean_shp_object$majhabs, useNA = "ifany")
  
  return(clean_shp_object)
}

# FN: GENERATE POLYGON UNIQUE IDs:
add_polygon_uids <- function(shape_object) {
  
  # Ascertain which polygons occupy the same space:
  equality_list <- sf::st_equals(shape_object)
  
  # Sort and capture a geometry sort index:
  shape_object <- shape_object %>% 
    dplyr::arrange(geometry) %>% 
    # Just keeping this row number for replicability/debugging
    dplyr::mutate(rown = row_number()) 
  
  # Generate polygon unique IDs:
  shape_object$pgn_id <- sapply(equality_list, FUN = function(x) x[1])
  
  return(shape_object)
}

# WRAPPER FN: 
cleanup_trimmed_data <- function(trimmed_shape_object) {
  # Clean column names
  clean_trimmed_object <- trimmed_shape_object %>% 
    clean_colnames() %>% 
    add_polygon_uids()
  
  return(clean_trimmed_object)
}



#### ORIGINAL CODE PASTED FOR REFERENCE BELOW HERE ####
# Temp for testing:
# raw_habmos <- st_read("data/HABMOS_SCOTLAND.shp")
# names(raw_habmos)
# cleaned_names_rh <- cleanup_colnames(raw_habmos)
# names(cleaned_names_rh)


# Original:
#### INTIAL PROCESSING, INC. ADDING UNIQUE POLYGON IDS ####
# trim_hab <- read_sf(file.path("data", "habmos_trimmed.shp"))
# colnames(trim_hab) <- tolower(colnames(trim_hab))
# names(trim_hab)
# 
# ## Need to make sure varnames are 7 characters or less to avoid carnage if 
# # writing out the shapefile. 
# # SYMBOL = major habitats
# trim_hab <- trim_hab %>%
#   rename(majhabs = symbol) %>%
#   rename(dethabs = habitat_co) %>%
#   rename(hab_nam = habitat_na) %>%
#   rename(source = habmos_sou) %>%
#   rename(svy_id = survey_id) %>%
#   rename(svy_dat = survey_dat) %>%
#   rename(hab_are = hab_area) %>% 
#   rename(svy_typ = survey_typ)
# names(trim_hab)
# 
# ## Polygon unique IDs:
# # Unfortunately, this id is only there for some of the data, and not even for 
# # all of the mosaics, e.g.:
# # trim_hab <- trim_hab %>% arrange(polygon_id)
# # missing_id <- trim_hab %>%
# #   filter(is.na(polygon_id))
# # View(missing_id)
# # table(missing_id$majhabs)
# # table(missing_id$source)
# # table(trim_hab$source)
# # Looks like Freshwater, Historic Landuse, Forestry and SGIACS do not ordinarily
# # have a polygon_id. 
# # table(missing_id$mosaic)
# # And many are mosaics
# # So we need to create a unique polygon id: 
# 
# ## Create a unique polygon id:
# 
# # Tried st_equals() and igraph approach to creating unique polygon ids:
# # This creates a list of the list of identical polygons for each polygon:
# equality_list <- st_equals(trim_hab)
# trim_hab <- trim_hab %>% 
#   arrange(geometry) %>% 
#   mutate(rown = row_number())
# equality_list
# equality_list[1]
# equality_list[10]
# equality_list[[10]][1]
# 
# # This part works well and is preferable to the string method below as it's 
# # more robust to slight changes in the way polygon geography is recorded. 
# 
# # Plan was to convert that list to a graph object:
# # g <- igraph::graph_from_adj_list(equality_list, mode = "all")
# # This ^ seemed to hang in an infinity loop. 
# # Not better than the other method.
# # Could be possible to use some logical way to check the output of the st_equals
# # object and gen polygon ids from that, but I'm not sure how to handle that 
# # list yet. 
# 
# # So the original method was to convert geometry to text and use that to 
# # identify uniques and duplicates. This takes a while, but leaving it for now:
# # trim_hab <- trim_hab %>%
# #   mutate(geomstring = st_as_text(geometry)) %>%
# #   mutate(pgn_id = as.numeric(as.factor(geomstring))) %>%
# #   select(-geomstring) %>%
# #   arrange(pgn_id)
# # summary(trim_hab$pgn_id)
# # head(trim_hab)
# 
# # But now trying a more logical operations approach:
# trim_hab$pgn_id <- sapply(equality_list, FUN = function(x) x[1])
# # Which seems to work OK. 
# 
# ## Double check 
# missing_id <- trim_hab %>%
#   filter(is.na(pgn_id))
# View(missing_id)
# # Sorted
# remove(missing_id)
# 
# st_write(trim_hab, file.path("data", "habmos_trimmed_with_id.shp"), append=FALSE)
# # REPLACED on 02Nov using the sapply/equality list method. 
# ####

# Added this bit from habitat classification on 14-11-2025 as it fits better in
# cleaning:
# Catch any EUNIS 1 (majhabs) entries that mean missing:
# table(forth_eunis$majhabs)
# 
# forth_eunis <- forth_eunis %>%
#   mutate(majhabs =
#            if_else(str_detect(majhabs, "NONE"), NA_character_, majhabs)) %>%
#   mutate(majhabs =
#            if_else(majhabs == "-", NA_character_, majhabs))
# 
# table(forth_eunis$majhabs, useNA = "ifany")
# sorted

