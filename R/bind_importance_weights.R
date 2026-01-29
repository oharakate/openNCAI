bind_importance_weights <- function(within_weights_list, es_label_tree) {

  # 1. Generate labels directly from the tree source
  all_es_labels <- unlist(es_label_tree, use.names = FALSE)

  # 2. Extract the 'weight' column from each data frame in the list
  # We use the names of the list to ensure we pull in the correct order
  combined_vector <- unlist(lapply(within_weights_list, function(df) df[["weight"]]))

  # 3. Check: does the length match?
  if(length(combined_vector) != length(all_es_labels)) {
    stop(paste0("Weight count (", length(combined_vector),
                ") does not match total service labels in tree (",
                length(all_es_labels), ")!"))
  }

  # 4. Convert to a single-row dataframe
  out <- as.data.frame(t(combined_vector))
  colnames(out) <- all_es_labels

  return(out)
}
