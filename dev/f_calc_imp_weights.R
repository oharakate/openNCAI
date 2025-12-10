## FUNCTION calc_imp_weights()
# Loops through the list of ecosystem service types, calculating importance
# weights and returning a list of weight subset objects.

# Requires the vector of between-service-type scores, and a list of the
# within-service-type-score objects.

calc_imp_weights <- function (between_scores, within_scores_list) {

  # Calculate the between weights
  b_weights <- imp_rtw_between(between_scores)

  # Initialise service type group number
  st_num <- 0

  # Initialise list of weight subsets
  ww_subset_list <- list()

  for (i in within_scores_list) {

    # Increment service type group number
    st_num <-  st_num + 1
    ww_subset_name <- paste0("ww_subset_", st_num)

    # Calculate within weights for service type
    ww_subset <- imp_rtw_within(scores = within_scores_list[[st_num]],
                                between_weights = b_weights,
                                index = st_num)
    assign(ww_subset_name, ww_subset, envir = .GlobalEnv)

    # Add the ww subset to the list thereof
    ww_subset_list[[ww_subset_name]] <- ww_subset

  }

  return(ww_subset_list)

}

