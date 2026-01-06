## FUNCTION bind_imp_weights()
# Rejoins within-service-type weights back into one weight vector, applying
# between-service-type weights.

# Require list of importance within weight vectors, output from imp_rtw_within()
# and list of all the service labels

bind_importance_weights <- function(within_weights_list, all_service_label_list) {

  # Row bind the subsets of weights
  long_weights <- dplyr::bind_rows(within_weights_list)

  if(nrow(long_weights) != length(all_service_label_list)) {
    stop("The number of rows in the weights (", nrow(long_weights),
         ") does not match the number of labels (", length(all_service_label_list), ").")
  }

  # Label rows with the service type subsets of service labels
  rownames(long_weights) <- all_service_label_list
  colnames(long_weights) <- ("weight")

  # Pivot wider to make one row df, services as cols
  wide_joined_weights <- as.data.frame(t(long_weights))

  return(wide_joined_weights)

}

