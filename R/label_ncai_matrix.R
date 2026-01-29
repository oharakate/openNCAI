#' Label NCAI Habitat/Ecosystem Service Matrix
#' This function takes a habitat/ecosystem service matrix of values used in
#' openNCAI and returns it as a data frame with the habitat and ecosystem
#' service labels applied.
#'
#' @param matrix A matrix of values, e.g. scores, weights, where rows are
#' habitats and columns are ecosystem services
##' @param habitats_label_tree A named list of character vectors where
#' each list name represents a broad habitat category, typically a EUNIS level
#' 1 habitat (e.g. "coastal") and the associated character vector contains the
#' labels or codes of the habitat sub-types - typically EUNIS level two
#' habitats - falling within that category (e.g., c("b1", "b2") or
#' c("coastal_dunes_sandy_shores", "coastal_shingle")). Syntactical names only
#' (no spaces or special characters).
#' The habitats label tree defines the relevant habitats for calculating the
#' NCAI and will be used to label the returned data frame.
#' @param es_label_tree A named list of character vectors where each list
#' name represents a label name for a type group of ecosystem services
#' (e.g. 'provisioning') and the associated character vector contains the
#' labels for the ecosystem services in that group (e.g. 'cultivated_crops').
#' Syntactical names only (no spaces or special characters).
#' The ES label tree defines the relevant ecosystem services for calculating
#' the NCAI and will be used to label the returned data frame.
#'
#' @return A labelled data frame.
#' @export
#'
#' @examples
#' # 1. Define the habitat tree (Total of 3 sub-habitats)
#' h_tree <- list(
#'   coastal = c("b1", "b2"),
#'   woodland = c("g1")
#' )
#'
#' # 2. Define the ecosystem service tree (Total of 2 services)
#' es_tree <- list(
#'   provisioning = c("crops", "timber")
#' )
#'
#' # 3. Create a raw matrix of values (3 rows x 2 columns)
#' raw_values <- matrix(
#'   c(1, 0.5, 0, 0, 0.2, 0.9),
#'   nrow = 3,
#'   ncol = 2
#' )
#'
#' # 4. Apply the labels
#' labeled_df <- label_ncai_matrix(raw_values, h_tree, es_tree)
#'
#' # View the result
#' print(labeled_df)
#' # Row names will be: "b1", "b2", "g1"
#' # Column names will be: "crops", "timber"
#'
label_ncai_matrix <- function(matrix,
                              habitats_label_tree,
                              es_label_tree) {

  flat_h <- unlist(habitats_label_tree, use.names = FALSE)
  flat_es <- unlist(es_label_tree, use.names = FALSE)

  if (length(flat_h) != nrow(matrix)) {
    stop("Number of habitat labels must match matrix rows.")
  }
  if (length(flat_es) != ncol(matrix)) {
    stop("Number of ES labels must match matrix columns.")
  }

  matrix <- as.data.frame(matrix)
  rownames(matrix) <- flat_h
  colnames(matrix) <- flat_es

  return(matrix)

}
