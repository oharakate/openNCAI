#' Bind Importance Weights to Service Labels
#'
#' Flattens a list of importance weight data frames into a
#' single-row data frame. \code{es_label_tree} is used to check the structure
#' is as expected.
#'
#' @param within_weights_list A named list of data frames, where each data frame
#' contains a \code{"weight"} column. Typically the output from
#' \code{calc_importance_weights}.
#' @param es_label_tree A named list of character vectors representing the
#' hierarchy of habitats (as character vectors, typically
#' EUNIS Level 2) within broad habitats (as list object names, typically EUNIS
#' Level 1). Syntactical names only (no spaces or special characters).
#' The names of this list must match names of \code{within_weights_list}.
#'
#' @return A data frame with a single row and columns named after the
#'   individual ecosystem services defined in \code{es_label_tree}.
#'
#' @details The function performs several safety checks:
#'   it verifies that all categories in the tree exist in the weights list,
#'   ensures the \code{"weight"} column is present in every data frame, and
#'   confirms that the total number of weights matches the total number of
#'   labels in the tree.
#'
#' @export
#'
#' @examples
#' # Define labels
#' es_tree <- list(
#'   provisioning = c("crops", "timber"),
#'   regulating = c("carbon")
#' )
#'
#' # Define weights list (note: the function is order-agnostic for the list)
#' w_list <- list(
#'   regulating = data.frame(weight = 0.5, row.names = "carbon"),
#'   provisioning = data.frame(weight = c(0.3, 0.2), row.names = c("crops", "timber"))
#' )
#'
#' # Bind weights to a single-row labeled data frame
#' final_weights <- bind_importance_weights(w_list, es_tree)
#' print(final_weights)
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
