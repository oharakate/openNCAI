#' Build Condition Indicator Weight Matrices (CIWM)
#'
#' Converts relevance matrices (CIRM) into weight matrices (CIWM) by
#' multiplying the relevance scores by the indicator's importance weights
#' (per ecosystem service type) as defined in the Indicator Directory.
#' @param cirm_list A named list of data frames. Names are condition indicator
#' labels. Data frames hold a matrix of binary values denoting whether the
#' Condition Indicator is relevant to each habitat/service combination. Names
#' must match the labels in \code{indicator_directory}.
#' @param indicator_directory A data frame containing weights
#' denoting the relevance of each condition indicator to each ecosystem service
#' type. Must include a column of condition indicator labels for matching.
#' @param es_label_tree A named list of character vectors representing
#' the hierarchy of ecosystem services (as character vectors) within
#' service type group (as list object names).
#' Syntactical names only (no spaces or special characters).
#' @param habitats_label_tree A named list of character vectors representing the
#' hierarchy of habitats (as character vectors, typically
#' EUNIS Level 2) within broad habitats (as list object names, typically EUNIS
#' Level 1). Syntactical names only (no spaces or special characters).
#' The habitats label tree is optional; if supplied a labelled data frame will
#' be returned.
#'
#' @importFrom utils stack
#' @importFrom stats setNames
#' @export
build_ciwm_list <- function(cirm_list,
                            indicator_directory,
                            es_label_tree,
                            habitats_label_tree = NULL) {

  # Make label_tree wide
  es_map <- stack(es_label_tree)

  ci_names <- names(cirm_list)

  final_ciwm_list <- lapply(ci_names, function(ci_id) {

    # Extract the binary relevance matrix (for speed)
    mat <- as.matrix(cirm_list[[ci_id]])

    # Get weights for this specific Indicator
    row_idx <- match(ci_id, indicator_directory$ci_id)
    if (is.na(row_idx)) stop(paste0("Indicator '", ci_id, "' not found in directory."))

    # Build weight vector
    weight_vec <- as.numeric(indicator_directory[row_idx, as.character(es_map$ind)])

    # MultiplY weight_vec across columns
    weighted_mat <- sweep(mat, 2, weight_vec, `*`)

    # Label matrices if both label trees passed in
    if (!is.null(habitats_label_tree)) {
      weighted_mat <- label_ncai_matrix(
        matrix = weighted_mat,
        habitats_label_tree = habitats_label_tree,
        es_label_tree = es_label_tree
      )
    }

    if (is.null(habitats_label_tree)) {
      # Ensure it's a data frame even if not labelled by the tree
      weighted_mat <- as.data.frame(weighted_mat)
    }

    return(weighted_mat)
  })

  return(setNames(final_ciwm_list, ci_names))
}
