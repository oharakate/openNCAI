# Writing a pair of functions which can pull out the time series of:
# - habitat extent * espb, the provision potential time series
# - habitat extent * wellbeing_base, the wellbeing potential time series

get_yearly_potential_provision <- function(habitat_extent,
                                           year_one,
                                           espb,
                                           as_matrices = FALSE) {

  years <-  colnames(habitat_extent)
  year_one_str <- as.character(year_one)

  extent_indices <- lapply(years,
                           get_habitat_extent_year_vec,
                           year_one = year_one,
                           habitat_extent = habitat_extent)

  yearly_provision_matrices <- setNames(
    lapply(extent_indices, function(idx_vec) {
      return(espb * idx_vec)
  }),
    years) # sets the names

  if (as_matrices == TRUE) {
    return(yearly_provision_matrices)
  } else {
    provision_time_series <- index_and_smooth(
      matrix_list = yearly_provision_matrices,
      year_one = year_one_str)

    return(provision_time_series)
  }
}





calc_weighted_habitat_extent <- function(habitat_extent,
                                         year_one,
                                         weight_matrix,
                                         as_matrices = FALSE) {

  years <-  colnames(habitat_extent)
  year_one_str <- as.character(year_one)

  extent_indices <- lapply(years,
                           get_habitat_extent_year_vec,
                           year_one = year_one_str,
                           habitat_extent = habitat_extent)

  list_of_matrices <- setNames(
    lapply(extent_indices, function(idx_vec) {
      return(weight_matrix * idx_vec)
    }),
    years) # sets the names

  if (as_matrices == TRUE) {
    return(list_of_matrices)

  } else {
    index_df <- index_and_smooth(
      matrix_list = list_of_matrices,
      year_one = year_one_str)

    return(index_df)
  }

}



get_yearly_potential_provision <- function(habitat_extent,
                                           year_one,
                                           espb,
                                           as_matrices = FALSE) {

  return(calc_weighted_habitat_extent(habitat_extent = habitat_extent,
                                      year_one = year_one,
                                      weight_matrix = espb,
                                      as_matrices = as_matrices))

}


get_yearly_potential_wellbeing <- function(habitat_extent,
                                           year_one,
                                           wellbeing_base,
                                           as_matrices = FALSE) {

  return(calc_weighted_habitat_extent(habitat_extent = habitat_extent,
                                      year_one = year_one,
                                      weight_matrix = wellbeing_base,
                                      as_matrices = as_matrices))

}

#try this out (must run get_ncai first):
prov_time_series_mats <- get_yearly_potential_provision(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  espb = ncai_objects$espb,
  as_matrices = TRUE)
# View(prov_time_series_mats[[1]])
# View(prov_time_series_mats[["2005"]])
str(prov_time_series_mats[[1]])
str(prov_time_series_mats[["2005"]])

prov_time_series <- get_yearly_potential_provision(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  espb = ncai_objects$espb,
  as_matrices = FALSE)
head(prov_time_series)


wb_time_series <- get_yearly_potential_wellbeing(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  wellbeing_base = ncai_objects$wellbeing_base,
  as_matrices = FALSE)
head(wb_time_series)

wb_time_series_mats <- get_yearly_potential_wellbeing(
  habitat_extent = ns_habitat_extent,
  year_one = 2000,
  wellbeing_base = ncai_objects$wellbeing_base,
  as_matrices = TRUE)
# View(wb_time_series_mats[[1]])
# View(wb_time_series_mats[["2005"]])
str(wb_time_series_mats[[1]])
str(wb_time_series_mats[["2005"]])


# A quick plot to see if they diverge:

# 1. Extract years and indices
years <- as.numeric(rownames(prov_time_series))
y1 <- prov_time_series$raw_index
y2 <- wb_time_series$raw_index

# 2. Create the plot with the first line
plot(years, y1, type = "o", col = "blue", pch = 16,
     ylim = range(c(y1, y2)), # Ensure both lines fit in the view
     xlab = "Year", ylab = "Raw Index",
     main = "Potential Provision vs Wellbeing Index")

# 3. Add the second line
lines(years, y2, type = "o", col = "darkgreen", pch = 17)

# 4. Add a legend
legend("topleft", legend = c("Provision", "Wellbeing"),
       col = c("blue", "darkgreen"), pch = c(16, 17), lty = 1)
