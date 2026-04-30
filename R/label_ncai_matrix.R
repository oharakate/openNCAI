#' @title Label NCAI Habitat/Ecosystem Service Matrix
#' This function takes a habitat/ecosystem service matrix of values used in
#' openNCAI and returns it as a data frame with the habitat and ecosystem
#' service labels applied.
#'
#' @param matrix A matrix of values, e.g. scores, weights, where rows are
#' habitats and columns are ecosystem services
#' @param habitats_label_tree A named list of character vectors representing the
#' hierarchy of habitats (as character vectors, typically
#' EUNIS Level 2) within broad habitats (as list object names, typically EUNIS
#' Level 1). Syntactical names only (no spaces or special characters).
#' @param es_label_tree A named list of character vectors representing
#' the hierarchy of ecosystem services (as character vectors) within
#' service type group (as list object names).
#' Syntactical names only (no spaces or special characters).
#'
#' @return A labelled data frame.
#' @export
#'
#' @examples
#' # 1. Define the habitat tree (Total of 3 sub-habitats)
#' h_tree <- list(
#'    coastal = c("b1", "b2"),
#'    woodland = c("g1")
#' )
#'
#' # 2. Define the ecosystem service tree (Total of 2 services)
#' es_tree <- list(
#'    provisioning = c("crops", "timber")
#' )
#'
#' # 3. Create a raw matrix of values (3 rows x 2 columns)
#' raw_values <- matrix(
#'    c(1, 0.5, 0, 0, 0.2, 0.9),
#'    nrow = 3,
#'    ncol = 2
#' )
#'
#' rownames(raw_values) <- c("b1", "b2", "g1")
#' colnames(raw_values) <- c("crops", "timber")
#'
#' # 4. Apply the labels
#' labeled_df <- label_ncai_matrix(matrix = raw_values,
#'                                 habitats_label_tree = h_tree,
#'                                 es_label_tree = es_tree)
#'
#'
#' # View the result
#' print(labeled_df)
#' # Row names will be: "b1", "b2", "g1"
#' # Column names will be: "crops", "timber"
#'
label_ncai_matrix <- function(matrix, habitats_label_tree, es_label_tree) {
  # check.names = FALSE prevents R from prepending 'X' to numeric IDs
  out <- as.data.frame(matrix, check.names = FALSE)

  flat_h <- unlist(habitats_label_tree, use.names = FALSE)
  flat_es <- unlist(es_label_tree, use.names = FALSE)

  # Dimension check: This is the only "strict" check we need.
  if (nrow(out) != length(flat_h) || ncol(out) != length(flat_es)) {
    stop(paste0("Dimension mismatch! Matrix is ", nrow(out), "x", ncol(out),
                ", but labels are ", length(flat_h), "x", length(flat_es)))
  }

  # FORCE the names to match the trees
  # This fixes the 'undefined columns' error by ensuring alignment
  rownames(out) <- flat_h
  colnames(out) <- flat_es

  return(out)
}
