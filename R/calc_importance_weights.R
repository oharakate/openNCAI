#' Calculate Importance Weights using Hierarchical Named Lists
#'
#' This function calculates the final importance weights for ecosystem services
#' by combining between-group scores (broad categories) and within-group scores
#' (specific services). It uses a name-aware approach, matching list names
#' to the provided label tree to ensure scores are applied to the correct categories.
#'
#' @param between_scores A named list of numeric values where names match the
#'   top-level categories in \code{es_label_tree}.
#' @param within_scores A named list where each element is itself a named
#'   list (or named vector) of numeric scores. The top-level names must match
#'   the categories in \code{es_label_tree}, and the inner names must match the
#'   specific ecosystem service labels.
#' @param es_label_tree A named list of character vectors representing the
#'   hierarchy of ecosystem services. This acts as the "source of truth" for
#'   ordering and selecting scores.
#'
#' @return A single-row data frame where columns are the individual ecosystem
#'   services and the values are their calculated importance weights,
#'   scaled by both between-group and within-group priorities.
#'
#' @details
#' The function first normalizes the between-group scores to 100. For each
#' category, it then normalizes the within-group scores and multiplies them
#' by the category's broad weight. If a category has a total score of zero,
#' all services within it are assigned a weight of 0.
#'
#' @export
#'
#' @examples
#' # 1. Define the Hierarchy
#' es_tree <- list(
#'   provisioning = c("crops", "timber"),
#'   regulating = c("carbon", "flood")
#' )
#'
#' # 2. Define Scores as named lists
#' # Note: The order of list elements does not matter as long as names match
#' b_scores <- list(regulating = 1, provisioning = 3)
#'
#' w_scores <- list(
#'   provisioning = list(timber = 5, crops = 10),
#'   regulating = list(carbon = 1, flood = 0)
#' )
#'
#' # 3. Run the calculation
#' importance_df <- calc_importance_weights(b_scores, w_scores, es_tree)
#' print(importance_df)
calc_importance_weights <- function(between_scores,
                                    within_scores,
                                    es_label_tree) {

  # 1. Calculate between-group weights
  # Extract scores matching the tree category names
  b_scores_vec <- unlist(between_scores[names(es_label_tree)])

  # Check for missing categories in between scores (in case of unlist behaviour)
  if (any(is.na(b_scores_vec)) || length(b_scores_vec) != length(es_label_tree)) {
    missing_b <- setdiff(names(es_label_tree), names(between_scores))
    stop(paste0("between_scores is missing required categories: ",
                paste(missing_b, collapse = ", ")))
  }

  b_total <- sum(b_scores_vec, na.rm = TRUE)
  if (b_total == 0) stop("Total of between_scores cannot be zero.")

  # Relative weight of each broad group (e.g., Provisioning = 75%)
  b_weights <- (b_scores_vec / b_total) * 100

  # 2. Calculate within-group weights.
  combined_vector <- unlist(lapply(names(es_label_tree), function(service_type) {

    # Check if the service_type category exists at all in the list
    if (!service_type %in% names(within_scores)) {
      stop(paste0("Category '", service_type, "' not found in within_scores."))
    }

    # Get score set and labels for this category
    w_scores_input <- within_scores[[service_type]]
    service_labels <- es_label_tree[[service_type]]

    # Check input labels are correct
    extracted_scores <- w_scores_input[service_labels]
    if (any(sapply(extracted_scores, is.null)) || any(is.na(unlist(extracted_scores)))) {
      stop(paste0("Specific service labels for '", service_type,
                  "' were not found in within_scores."))
    }

    # Get total for labels defined in the tree
    w_scores <- unlist(w_scores_input[service_labels])
    w_total <- sum(w_scores, na.rm = TRUE)

    # 3. Calculate final weights:
    # (within-group proportion) * (between-group weight)
    if (w_total == 0) {
      importance_weights <- rep(0, length(w_scores))
    } else {
      importance_weights <- (w_scores / w_total) * b_weights[service_type]
    }

    return(importance_weights)
  }))

  # Final formatting
  all_es_labels <- unlist(es_label_tree, use.names = FALSE)

  # Catch any remaining mismatch of length
  if (length(combined_vector) != length(all_es_labels)) {
    stop("Weight calculation resulted in a dimension mismatch.")
  }

  # Convert the flattened vector into a single-row data frame
  out <- as.data.frame(t(combined_vector))
  colnames(out) <- all_es_labels

  return(out)
}
