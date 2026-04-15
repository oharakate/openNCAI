# Writing a pair of functions which can pull out the time series of:
# - habitat extent * espb, the provision potential time series
# - habitat extent * wellbeing_base, the wellbeing potential time series

get_provision_time_series <- function(habitat_extent, year_one, espb) {

  years <-  colnames(habitat_extent)

  extent_indices <- lapply(years,
                           get_habitat_extent_year_vec,
                           year_one = year_one,
                           habitat_extent = habitat_extent)

  provision_time_series <- lapply(extent_indices, function(idx_vec) {
    return(espb * idx_vec)
  })

  return(provision_time_series)
}


# helper function to get one year habitat vector
# nb returns values indexed on year one
get_habitat_extent_year_vec <- function(target_year, year_one, habitat_extent) {

  # Convert to characters for safe indexing
  target_str <- as.character(target_year)
  origin_str <- as.character(year_one)

  # Extract extent vectors
  extent_target_vec <- habitat_extent[, target_str]
  extent_origin_vec <- habitat_extent[, origin_str]

  # Index the habitat extent values (Target / Origin * 100)
  extent_index <- (extent_target_vec / extent_origin_vec * 100)

  return(extent_index)

}

#try this out (must run get_ncai first):
prov_time_series <- get_provision_time_series(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  espb = ncai_objects$espb)
View(prov_time_series[[1]])
