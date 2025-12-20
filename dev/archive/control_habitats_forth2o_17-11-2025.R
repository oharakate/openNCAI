## BETA This control script runs all functions to trim, clean, classify and 
## render-for-mapping HabMoS data
## Kate O'Hara 
## 17-11-2025

## CROP HABMOS TO AOI, CLEAN, CLASSIFY, MAP AND OPTIONALLY CALCULATE LOCAL NCAI
working_data <- crop_and_intersect(
  "enter_input_data_path", 
  "enter_aoi_data_path") %>% 
  
  clean_trimmed_data() %>% 

  classify_habitats(scheme = "all") %>% 
  # scheme is default "all" and may be:
  # eunis1     (all eunis1 habitats for map)
  # ncai_broad (NCAI seven broad habitats, as usually reported, for map)
  # ncai_full  (NCAI all broad habitats, for calculation of regional NCAI)

  make_map_layers(
    coverage = TRUE, 
    eunis1 = TRUE,
    ncai = TRUE)
  # coverage, eunis1 and ncai are TRUE by default
  # note that ncai_full is not mapped and it is ncai_broad which is mapped

  # %T% 
  # code to calculate regional NCAI would go here, receiving the 
  # clean_trimmed_data (though the passing out of the clean data is currently
  # hard coded into make_map_layers() - we could change that.)
  # ncaibh2 and ncaisht are the variables classifying NCAI broad habs fully.

