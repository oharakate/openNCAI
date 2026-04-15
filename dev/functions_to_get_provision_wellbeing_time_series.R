# Writing a pair of functions which can pull out the time series of:
# - habitat extent * espb, the provision potential time series
# - habitat extent * wellbeing_base, the wellbeing potential time series

get_yearly_potential_provision <- function(habitat_extent, year_one, espb) {

  years <-  colnames(habitat_extent)

  extent_indices <- lapply(years,
                           get_habitat_extent_year_vec,
                           year_one = year_one,
                           habitat_extent = habitat_extent)

  provision_time_series <- setNames(
    lapply(extent_indices, function(idx_vec) {
      return(espb * idx_vec)
  }),
    years) # sets the names

  return(provision_time_series)
}



#try this out (must run get_ncai first):
# prov_time_series <- get_yearly_potential_provision(
#   habitat_extent = ns_habitat_extent,
#   year_one = 2000,
#   espb = ncai_objects$espb)
# View(prov_time_series[[1]])
# View(prov_time_series[["2005"]])
