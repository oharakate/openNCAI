  ## BETA This script holds functions to classify habitat data from the Habitats
  ## Map of Scotland (HabMoS) as EUNIS1 habitats or broad habitats used in 
  ## calculating Scotland's Natural Capital Assets Index.
  ## Kate O'Hara 
  ## 05-11-2025


# This script will take a shapefile which has been trimmed to the area of 
# interest and has been cleaned and given polygon unique IDs.

# FN: EUNIS LEVEL 1 CLASSIFICATION
# Collect EUNIS level 1 categorisations
classify_as_eunis1 <- function(shape_object) {
  
  # Drop missing data:
  classified_data <- shape_object %>% 
    dplyr::filter(
      !is.na(majhabs)
    ) %>% 
  
  # Create numbered EUNIS1 classification:
    dplyr::mutate(
      eu1num = dplyr::case_when(
        substr(majhabs, 1, 1) == "A" ~ 1,
        substr(majhabs, 1, 1) == "B" ~ 2,
        substr(majhabs, 1, 1) == "C" ~ 3,
        substr(majhabs, 1, 1) == "D" ~ 4,
        substr(majhabs, 1, 1) == "E" ~ 5,
        substr(majhabs, 1, 1) == "F" ~ 6,
        substr(majhabs, 1, 1) == "G" ~ 7,
        substr(majhabs, 1, 1) == "H" ~ 8,
        substr(majhabs, 1, 1) == "I" ~ 9,
        substr(majhabs, 1, 1) == "J" ~ 10,
        substr(majhabs, 1, 1) == "X" ~ 11,
        
        # Catch any unmatched
        TRUE ~ NA_character_
      )
    ) %>% 
  
  # Capture EUNIS1 letter code:
    dplyr::mutate(
      eu1let = substr(majhabs, 1, 1)
    ) %>%
    
  # Capture EUNIS1 category name:
    dplyr::mutate(
      eunis1 = substr(majhabs, 5, nchar(majhabs))
    ) %>% 
    
  # Extra check for "No information" string - maybe redundant 
    mutate(eunis1 = if_else(str_detect(eunis1, "No information")
                            , NA_character_, eunis1)) %>% 
    
  # Apply Forth2O Explore map legend labels
    dplyr::mutate(
      eu1lab = dplyr::case_when(
        substr(majhabs, 1, 1) == "A" ~ "Marine",
        substr(majhabs, 1, 1) == "B" ~ "Coastal",
        substr(majhabs, 1, 1) == "C" ~ "Freshwater",
        substr(majhabs, 1, 1) == "D" ~ "Wetland",
        substr(majhabs, 1, 1) == "E" ~ "Grassland",
        substr(majhabs, 1, 1) == "F" ~ "Heathland",
        substr(majhabs, 1, 1) == "G" ~ "Woodland",
        substr(majhabs, 1, 1) == "H" ~ "Unvegetated",
        substr(majhabs, 1, 1) == "I" ~ "Cropland",
        substr(majhabs, 1, 1) == "J" ~ "Built",
        substr(majhabs, 1, 1) == "X" ~ "Mixed",
        
        # Catch any unmatched
        TRUE ~ NA_character_
      )
    ) %>% 
  
    dplyr::group_by(pgn_id) %>% 
    dplyr::mutate(is_eunis1_mosaic = (n_distinct(majhabs, na.rm = TRUE) > 1)) %>% 
    dplyr::ungroup()

  return(classified_data)
}


# FN: NCAI 'BROAD HABITATS' CLASSIFICATION
# Collect 7 broad habitats as highlighted in NatureScot publications.
# NB this ignores some habitats which do contribute to NCAI calculation. 
# This classification is primarily for Forth2O Explore mapping.
classify_as_ncai_broad <- function(shape_object) {
  
  classified_data <- shape_object %>% 
    dplyr::mutate(
      brodhab = dplyr::case_when(
        substr(dethabs, 1, 2) == "B1" ~ "Coastal",
        substr(dethabs, 1, 2) == "B2" ~ "Coastal",
        substr(dethabs, 1, 2) == "B3" ~ "Coastal",
        substr(dethabs, 1, 1) == "C"  ~ "Freshwater", # Note, different! #
        substr(dethabs, 1, 2) == "D1" ~ "Wetland",
        substr(dethabs, 1, 2) == "D2" ~ "Wetland",
        substr(dethabs, 1, 2) == "D3" ~ "Wetland",
        substr(dethabs, 1, 2) == "D4" ~ "Wetland",
        substr(dethabs, 1, 2) == "E1" ~ "Grassland",
        substr(dethabs, 1, 2) == "E2" ~ "Grassland",
        substr(dethabs, 1, 2) == "E4" ~ "Grassland",
        substr(dethabs, 1, 2) == "E5" ~ "Grassland",
        substr(dethabs, 1, 2) == "E7" ~ "Grassland",
        substr(dethabs, 1, 2) == "F2" ~ "Heathland",
        substr(dethabs, 1, 2) == "F3" ~ "Heathland",
        substr(dethabs, 1, 2) == "F4" ~ "Heathland",
        substr(dethabs, 1, 2) == "F9" ~ "Heathland",
        substr(dethabs, 1, 2) == "G1" ~ "Woodland",
        substr(dethabs, 1, 2) == "G3" ~ "Woodland",
        substr(dethabs, 1, 2) == "G4" ~ "Woodland",
        substr(dethabs, 1, 2) == "G5" ~ "Woodland",
        substr(dethabs, 1, 2) == "G6" ~ "Woodland",
        substr(dethabs, 1, 2) == "I1" ~ "Cropland",
        substr(dethabs, 1, 2) == "I2" ~ "Cropland",
        
        # Catch any unmatched
        TRUE ~ NA_character_
        
        # IMPORTANT FOR REGIONAL NCAI - 
        # May need to add in bit to deal with H, J, K, X
      )
    ) %>%
      
      dplyr::mutate(
        brodnum = dplyr::case_when(
          brodhab == "Coastal" ~ 1L,
          brodhab == "Freshwater" ~ 2L,
          brodhab == "Wetland" ~ 3L,
          brodhab == "Grassland" ~ 4L,
          brodhab == "Heathland" ~ 5L,
          brodhab == "Woodland" ~ 6L,
          brodhab == "Cropland" ~ 7L,
          
          # Catch unmatched:
          TRUE ~ NA_integer_
        )
      ) %>%
    
    # Mark if polygon is a NCAI mosaic, for mapping.
      dplyr::group_by(pgn_id) %>% 
      dplyr::mutate(is_ncai_mosaic = (n_distinct(brodhab, na.rm = TRUE) > 1)) %>% 
      dplyr::ungroup()
  
  return(classified_data)
}


## FN: (proposed) NCAI FULL CLASSIFICATION
# This function would classify polygons under all habitats contributing to 
# the NCAI calculation. It is there for intended for replicating calculation
# of the index, as opposed to classify_as_ncai_broad() above which only 
# identifies the seven broad habitat's highlighted in NCAI publications. 
# At the moment, it just 
classify_as_ncai_full <- function(shape_object) {
  classified_data <- shape_object %>% 
    
    dplyr::mutate(
      ncaibh2 = dplyr::case_when(
        substr(dethabs, 1, 2) == "B1" ~ "Coastal",
        substr(dethabs, 1, 2) == "B2" ~ "Coastal",
        substr(dethabs, 1, 2) == "B3" ~ "Coastal",
        substr(dethabs, 1, 1) == "C"  ~ "Freshwater", # Note, different! #
        substr(dethabs, 1, 2) == "D1" ~ "Wetland",
        substr(dethabs, 1, 2) == "D2" ~ "Wetland",
        substr(dethabs, 1, 2) == "D3" ~ "Wetland",
        substr(dethabs, 1, 2) == "D4" ~ "Wetland",
        substr(dethabs, 1, 2) == "E1" ~ "Grassland",
        substr(dethabs, 1, 2) == "E2" ~ "Grassland",
        substr(dethabs, 1, 2) == "E4" ~ "Grassland",
        substr(dethabs, 1, 2) == "E5" ~ "Grassland",
        substr(dethabs, 1, 2) == "E7" ~ "Grassland",
        substr(dethabs, 1, 2) == "F2" ~ "Heathland",
        substr(dethabs, 1, 2) == "F3" ~ "Heathland",
        substr(dethabs, 1, 2) == "F4" ~ "Heathland",
        substr(dethabs, 1, 2) == "F9" ~ "Heathland",
        substr(dethabs, 1, 2) == "G1" ~ "Woodland",
        substr(dethabs, 1, 2) == "G3" ~ "Woodland",
        substr(dethabs, 1, 2) == "G4" ~ "Woodland",
        substr(dethabs, 1, 2) == "G5" ~ "Woodland",
        substr(dethabs, 1, 2) == "G6" ~ "Woodland",
        substr(dethabs, 1, 2) == "H2" ~ "Unvegetated",
        substr(dethabs, 1, 2) == "H3" ~ "Unvegetated",
        substr(dethabs, 1, 2) == "I1" ~ "Cropland",
        substr(dethabs, 1, 2) == "I2" ~ "Cropland",
        substr(dethabs, 1, 2) == "J1" ~ "Built",
        substr(dethabs, 1, 2) == "J2" ~ "Built",
        substr(dethabs, 1, 2) == "J3" ~ "Built",
        substr(dethabs, 1, 2) == "J4" ~ "Built",
        substr(dethabs, 1, 1) == "K"  ~ "Montane",
        
        # Catch any unmatched
        TRUE ~ NA_character_
        # This captures all level 2 habitats listed in column E of NCAI
        # spreadsheet. 
      ) 
    ) %>%
    
    dplyr::mutate(
      hasncful = as.integer(!is.na(ncaibh2))
    ) %>% 
    
    dplyr::mutate(
      ncaisht = dplyr::if_else(
        hasncful == 1L,
        trimws(substr(dethabs, 1, 2)),
        NA_character_
      )
    ) 
    
    
  return(classified_data)
}



## FN: APPLY LABELS
# This function optional. Applies labels to the newly created classification 
# labels. Perhaps in use this is more likely to be wanted inside other 
# parts of the process. Will leave it hear for now, but it might live in its 
# own script eventually. 
label_classifications <- function(classified_shapefile) {
  
  labelled_shapefile <- classified_shapefile %>% 
    labelled::set_variable_labels(
      brodhab = "NCAI Broad Habitat",
      brodnum = "NCAI broad habitat number",
      eunis1 = "EUNIS 1 descriptive name",
      eu1let = "EUNIS 1 letter code",
      eu1num = "Our EUNIS 1 number",
      eu1lab = "Our EUNIS 1 short label",
      ncaibh2 = "Our full NCAI short name",
      ncaisht = "Short code if in full NCAI",
      hasncful = "Habitat is counted in full NCAI"
    ) 
  
  return(labelled_shapefile)
}


## FN: WRAPPER
# This function should accept a trimmed, cleaned and processed shapefile,
# and an argument to determine which classification is applied.
# Alternatively all classifications could be applied? 
# Maybe the default can be all, with the option to do just one?
classify_habitats <- function(shape_object, scheme = "all") {
  
  if (scheme == "all") {
    # Run the full set of classification functions (e.g., full NCAI, EUNIS, etc.)
    classified_data <- shape_object %>% 
      classify_as_eunis1() %>% 
      classify_as_ncai_broad() %>% 
      classify_as_ncai_full() 
    
  } else if (scheme == "eunis1") {
    # Run only the broad habitat classification
    classified_data <- shape_object %>% 
      classify_as_eunis1()
    
  } else if (scheme == "ncai_broad") {
    # Run only the broad habitat classification
    classified_data <- shape_object %>% 
      classify_as_ncai_broad()
    
  } else if (scheme == "ncai_full") {
    # Run only the broad habitat classification
    classified_data <- shape_object %>% 
      classify_as_ncai_full() 
    
  } else {
    stop("Invalid classification scheme specified. Use 'all', 'ncai_full', 'ncai_broad' or 'eunis1'.")
  }
  
  # Optional labelling (on the understanding that it shouldn't break if 
  # some of the variables in this label scheme are not found in the data, e.g.
  # if scheme!="all".)
  
  classified_data <- classified_data %>% 
    label_classifications()
  
  return(classified_data)
}



#### ORIGINAL CODE PASTED FOR REFERENCE BELOW HERE ####

#### CLASSIFYING AS NCAI SEVEN BROAD HABITATS PER NCAI ####
# forth_eunis <- read_sf(file.path("data", "habmos_trimmed_with_id.shp"))
# names(forth_eunis)

## 14 Aug classifying manually according to NatureScot spreadsheet.

# Checking if all EUNIS here is covered by NatureScot details:
# arrange(forth_eunis, dethabs)
# table(forth_eunis$dethabs)
# Some E3, H5, J6, X, A* (Marine) which are all not in NatureScot NCAI sheet. 
# Also some with no information. No apparent K. 
# However, I have manually checked that for B, C, D, F, G and I, all 
# entries of that group can be classified using only their first letter.
# Nonetheless, hardcoding 2-character approach in case other regions have 
# different subcategories. 

## BEST DOUBLE CHECK THIS IF THE BOUNDARIES CHANGE AGAIN THOUGH. 
# REMEMBER THIS IS JUST THE COMMONLY REPORTED ONES.
# FOR CALCULATION MONTANE FOR EXAMPLE SHOULD BE INCLUDED
# Hard coded all sub cats 2 Nov 25.

# forth_eunis <- forth_eunis %>%
#   
#   mutate(
#     brodhab = case_when(
#       str_sub(dethabs, 1, 2) == "B1" ~ "Coastal",
#       str_sub(dethabs, 1, 2) == "B2" ~ "Coastal",
#       str_sub(dethabs, 1, 2) == "B3" ~ "Coastal",
#       str_sub(dethabs, 1, 1) == "C"  ~ "Freshwater", # Note, different! #
#       str_sub(dethabs, 1, 2) == "D1" ~ "Wetland",
#       str_sub(dethabs, 1, 2) == "D2" ~ "Wetland",
#       str_sub(dethabs, 1, 2) == "D3" ~ "Wetland",
#       str_sub(dethabs, 1, 2) == "D4" ~ "Wetland",
#       str_sub(dethabs, 1, 2) == "E1" ~ "Grassland",
#       str_sub(dethabs, 1, 2) == "E2" ~ "Grassland",
#       str_sub(dethabs, 1, 2) == "E4" ~ "Grassland",
#       str_sub(dethabs, 1, 2) == "E5" ~ "Grassland",
#       str_sub(dethabs, 1, 2) == "E7" ~ "Grassland",
#       str_sub(dethabs, 1, 2) == "F2" ~ "Heathland",
#       str_sub(dethabs, 1, 2) == "F3" ~ "Heathland",
#       str_sub(dethabs, 1, 2) == "F4" ~ "Heathland",
#       str_sub(dethabs, 1, 2) == "F9" ~ "Heathland",
#       str_sub(dethabs, 1, 2) == "G1" ~ "Woodland",
#       str_sub(dethabs, 1, 2) == "G3" ~ "Woodland",
#       str_sub(dethabs, 1, 2) == "G4" ~ "Woodland",
#       str_sub(dethabs, 1, 2) == "G5" ~ "Woodland",
#       str_sub(dethabs, 1, 2) == "G6" ~ "Woodland",
#       str_sub(dethabs, 1, 2) == "I1" ~ "Cropland",
#       str_sub(dethabs, 1, 2) == "I2" ~ "Cropland"
#       
#       # IMPORTANT FOR REGIONAL NCAI - 
#       # May need to add in bit to deal with H, J, K, X
#     )
#   ) %>%
#   
#   mutate(
#     brodnum = case_when(
#       brodhab == "Coastal" ~ 1L,
#       brodhab == "Freshwater" ~ 2L,
#       brodhab == "Wetland" ~ 3L,
#       brodhab == "Grassland" ~ 4L,
#       brodhab == "Heathland" ~ 5L,
#       brodhab == "Woodland" ~ 6L,
#       brodhab == "Cropland" ~ 7L
#     )
#   )
# 
# 
# # Check:
# # Only NCAI numbered in brodnum
# # subby <- forth_eunis %>%
# #   select(brodhab, dethabs, brodnum) %>%
# #   filter(is.na(brodhab))
# # View(subby)
# # table(subby$brodnum)
# # table(forth_eunis$brodnum, useNA = "ifany")
# # remove(subby)
# # fine
# 
# # Cehck: only the level 2 EUNIS cats listed in NatureScot spreadsheet 
# # categorised  as brodhab are here:
# # bhablist <- forth_eunis %>%
# #   pull(brodhab) %>%
# #   unique()
# # bhablist
# # 
# # for (i in bhablist) {
# #   dhabs <- forth_eunis %>%
# #     arrange(dethabs) %>%
# #     filter(brodhab==i) %>%
# #     pull(dethabs) %>%
# #     unique()
# #   print(paste0("Broad habitat: ", i))
# #   print(dhabs)
# #   print("")
# # }
# # Looks good.
# 
# ## The unadjusted EUNIS1 categories should be easy enough. Might already be 
# # symbol? Is first character of this.
# table(forth_eunis$majhabs, useNA = "ifany")
# 
# forth_eunis <- forth_eunis %>%
#   mutate(
#     eu1num = case_when(
#       majhabs=="NONE - No information" ~ NA,
#       majhabs=="-" ~ NA,
#       substr(majhabs, 1, 1) == "A" ~ 1,
#       substr(majhabs, 1, 1) == "B" ~ 2,
#       substr(majhabs, 1, 1) == "C" ~ 3,
#       substr(majhabs, 1, 1) == "D" ~ 4,
#       substr(majhabs, 1, 1) == "E" ~ 5,
#       substr(majhabs, 1, 1) == "F" ~ 6,
#       substr(majhabs, 1, 1) == "G" ~ 7,
#       substr(majhabs, 1, 1) == "H" ~ 8,
#       substr(majhabs, 1, 1) == "I" ~ 9,
#       substr(majhabs, 1, 1) == "J" ~ 10,
#       substr(majhabs, 1, 1) == "X" ~ 11
#     )
#   ) %>%
#   
#   filter(
#     !is.na(majhabs)
#   ) %>%
#   
#   mutate(
#     eu1let = substr(majhabs, 1, 1)
#   ) %>%
#   
#   mutate(
#     eunis1 = substr(majhabs, 5, nchar(majhabs))
#   ) %>%
#   
#   mutate(
#     eu1lab = case_when(
#       substr(majhabs, 1, 1) == "A" ~ "Marine",
#       substr(majhabs, 1, 1) == "B" ~ "Coastal",
#       substr(majhabs, 1, 1) == "C" ~ "Freshwater",
#       substr(majhabs, 1, 1) == "D" ~ "Wetland",
#       substr(majhabs, 1, 1) == "E" ~ "Grassland",
#       substr(majhabs, 1, 1) == "F" ~ "Heathland",
#       substr(majhabs, 1, 1) == "G" ~ "Woodland",
#       substr(majhabs, 1, 1) == "H" ~ "Unvegetated",
#       substr(majhabs, 1, 1) == "I" ~ "Cropland",
#       substr(majhabs, 1, 1) == "J" ~ "Built",
#       substr(majhabs, 1, 1) == "X" ~ "Mixed"
#     )
#   )
# 
# forth_eunis <- forth_eunis %>%
#   set_variable_labels(
#     brodhab = "NCAI Broad Habitat",
#     brodnum = "NCAI broad habitat number",
#     eunis1 = "EUNIS 1 descriptive name",
#     eu1let = "EUNIS 1 letter code",
#     eu1num = "Our EUNIS 1 number",
#     eu1lab = "Our EUNIS 1 short label"
#   )
# var_label(forth_eunis)
# 
# 
# 
# # Save out with brodhabs:
# # (Last run on 05-Nov)
# st_write(forth_eunis, file.path("data", "feunis_brodhabs.shp"), append=FALSE)
# remove(forth_eunis)
# remove(aoi)
# remove(box_feunis)
# remove(intersect_feunis)
# remove(equality_list)
# remove(test)
# remove(trim_hab)
# remove(eunis)
# remove(unique_polygon_count)
# ####

