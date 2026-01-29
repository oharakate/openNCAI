#' Calculate Ecosystem Service Importance Weights
#'
#' Calculate the importance weights (both within- and between-ecosystem-service-type)
#' from Ecosystem Service (ES) importance scores. This function scales scores so
#' that the total weight across all services sums to 100, partitioned by the
#' relative importance of the service types defined in `es_label_tree`.
#'
#' @param between_scores A data frame containing the importance scores for broad
#' ecosystem service categories (e.g., Provisioning, Regulating). Rows must
#' correspond to the top-level names in `es_label_tree`.
#' @param between_colname Character. The name of the numeric column in
#' `between_scores` containing the scores. Defaults to "score".
#' @param within_scores_list A named list of data frames. Each list element
#' represents a service type and contains a data frame of scores for individual
#' services. List names must align with the service types in `es_label_tree`.
#' Row names in these data frames are not required as they are applied from
#' `es_label_tree`.
#' @param within_colname Character. The name of the numeric column in the
#' data frames within `within_scores_list`. Defaults to "score".
#' @param es_label_tree A named list of character vectors defining the
#' hierarchy and labels of ecosystem services. This tree is used to label the
#' resulting weight data frames.
#'
#' @return A named list of data frames, where each data frame contains a
#' `weight` column and row names derived from `es_label_tree`. The total of
#' all weights across the entire list will sum to 100.
#' @export
#'
#' @examples
#' # Define labels
#' es_tree <- list(
#'   provisioning = c("crops", "timber"),
#'   regulating = c("carbon", "flood")
#' )
#'
#' # Define broad category importance (e.g., Regulating is twice as important)
#' b_scores <- data.frame(
#'   score = c(1, 2),
#'   row.names = c("provisioning", "regulating")
#' )
#'
#' # Define individual service importance (raw scores, no row names needed)
#'   w_list <- list(provisioning = data.frame(score = c(0.5, 0.5)),
#'   regulating = data.frame(score = c(0.8, 0.2))
#' )
#'
#' # Calculate weights
#' weights <- calc_importance_weights(
#'   between_scores = b_scores,
#'   within_scores_list = w_list,
#'   es_label_tree = es_tree
#' )
#'
#' # View results
#' print(weights)
calc_importance_weights <- function(between_scores,
                                    between_colname = "score",
                                    within_scores_list,
                                    within_colname = "score",
                                    es_label_tree) {

  # 1. Calculate between-group weights
  # Sum check to avoid NaN if all between-scores are zero
  b_total <- sum(between_scores[[between_colname]], na.rm = TRUE)
  if (b_total == 0) stop("Total of between_scores cannot be zero.")

  b_weights <- (between_scores[[between_colname]] / b_total) * 100
  names(b_weights) <- names(es_label_tree)

  # 2. Align list names to ensure mapping works
  names(within_scores_list) <- names(es_label_tree)

  # 3. Calculate within-group weights
  iw_subset_list <- lapply(names(es_label_tree), function(service_type) {

    w_scores <- within_scores_list[[service_type]]
    service_labels <- es_label_tree[[service_type]]

    # Make sure the number of scores matches the number of labels in the tree
    if (nrow(w_scores) != length(service_labels)) {
      stop(paste0("Number of scores for '", service_type,
                  "' does not match the number of labels in es_label_tree."))
    }

    w_total <- sum(w_scores[[within_colname]], na.rm = TRUE)

    # Handle the case where a category has only zero scores
    if (w_total == 0) {
      importance_weights <- rep(0, nrow(w_scores))
    } else {
      importance_weights <- (w_scores[[within_colname]] / w_total) * b_weights[service_type]
    }

    # Returning as a data frame is usually safer for your other functions
    return(data.frame(
      weight = importance_weights,
      row.names = service_labels
    ))
  })

  names(iw_subset_list) <- names(es_label_tree)
  return(iw_subset_list)
}
