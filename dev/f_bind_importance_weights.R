## FUNCTION bind_imp_weights()
# Rejoins within-service-type weights back into one weight vector, applying
# between-service-type weights.

# Require list of importance within weight vectors, output from imp_rtw_within()
# and list of all the service labels

bind_importance_weights <- function(within_weights_list,
                                    all_service_label_list) {

  # Safely get each subset of weights and flatten to one vector
  combined_weights <- unlist(lapply(within_weights_list, `[[`, 1), use.names = FALSE)

  # Check the list of weights is now the same length as list of all services
  if(length(combined_weights) != length(all_service_label_list)) {
    stop("Length mismatch: Total weights (", length(combined_weights),
         ") vs Labels (", length(all_service_label_list), ").")
  }

  # Put in wide format
  wide_joined_weights <- as.data.frame(t(combined_weights))
  colnames(wide_joined_weights) <- all_service_label_list

  return(wide_joined_weights)
}

