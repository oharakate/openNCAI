bind_importance_weights <- function(within_weights_list, es_label_tree) {

  # Check all sections are there and that they have a "weight" column
  lapply(names(es_label_tree), function(es_type) {
    if (!es_type %in% names(within_weights_list)) {
      stop(paste0("The ES type '", es_type, "' from the tree is missing from your weight list."))
    }
    if (!"weight" %in% colnames(within_weights_list[[es_type]])) {
      stop(paste0("The ecosystem service type '", es_type,
                  "' is missing the required 'weight' column."))
    }
  })

  # Extract weights in order of the ES label tree.
  combined_vector <- unlist(lapply(names(es_label_tree), function(es_type) {
    within_weights_list[[es_type]][["weight"]]
  }))

  # Get the service labels
  all_es_labels <- unlist(es_label_tree, use.names = FALSE)

  # Check dimensions match
  if(length(combined_vector) != length(all_es_labels)) {
    stop(paste0("Weight count (", length(combined_vector),
                ") does not match labels in tree (",
                length(all_es_labels), ")!"))
  }

  # Output as single-row dataframe
  out <- as.data.frame(t(combined_vector))
  colnames(out) <- all_es_labels

  return(out)
}
