#' Show where a single object has missing data
#'
#' @description
#' Shows missing data for one openNCAI pipeline object, chosen by
#' \code{\link{check_missing}} as the follow-up step once it's flagged
#' something. Data frames (including individual elements of
#' \code{ci_relevance_matrices}, e.g. \code{ci_relevance_matrices$some_ci})
#' are shown as a bar chart of \% missing per row, using ggplot2. Named
#' lists (label trees and weight trees) and bare vectors are shown as a
#' plain-text listing of which specific entries are blank or missing,
#' via \code{message()}, since a bar chart doesn't apply to those shapes.
#'
#' @param x The object to check: a data frame, named list, or vector.
#' @param label_col Optional column name to use as the row label instead
#'   of \code{rownames(x)}, for data frames whose row names aren't
#'   meaningful (e.g. \code{indicator_directory}, which should be shown
#'   with \code{label_col = "ci_id"}).
#'
#' @return For data frames with missing data, a ggplot2 plot object. For
#'   complete data frames, and for lists and vectors, invisibly
#'   \code{NULL} (the listing, if any, is printed via \code{message()}).
#' @export
#'
#' @examples
#' show_missing(ns_habitats_label_tree)
show_missing <- function(x, label_col = NULL) {
  if (is.data.frame(x)) {
    return(.show_missing_df(x, label_col = label_col))
  }
  if (is.list(x)) {
    return(.show_missing_tree(x))
  }
  if (is.atomic(x)) {
    return(.show_missing_vector(x))
  }
  stop("show_missing() doesn't know how to handle an object of class ", paste(class(x), collapse = "/"), ".")
}

#' @noRd
.show_missing_df <- function(df, label_col = NULL) {
  smry <- .summarise_df(df, label_col = label_col, top_n = nrow(df))
  if (smry$missing == 0) {
    message("✓ Complete data supplied.")
    return(invisible(NULL))
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for show_missing() on data frames. ",
         "Install it with install.packages('ggplot2').")
  }

  empty_mat <- as.matrix(as.data.frame(lapply(df, .is_empty_value)))
  labels <- if (!is.null(label_col)) as.character(df[[label_col]]) else rownames(df)
  pct <- rowSums(empty_mat) / ncol(df) * 100

  plot_df <- data.frame(label = labels, pct = pct)
  plot_df <- plot_df[order(plot_df$pct, decreasing = TRUE), ]
  plot_df$label <- factor(plot_df$label, levels = rev(plot_df$label))

  ggplot2::ggplot(plot_df) +
    ggplot2::geom_col(ggplot2::aes(y = .data$label, x = .data$pct, fill = .data$pct)) +
    ggplot2::scale_fill_viridis_c(guide = "none") +
    ggplot2::theme_classic(base_size = 18) +
    ggplot2::xlab("% of values which are missing.") +
    ggplot2::ylab(NULL)
}

#' @noRd
.show_missing_tree <- function(x) {
  smry <- .summarise_tree(x)
  if (smry$missing == 0) {
    message("✓ Complete data supplied.")
    return(invisible(NULL))
  }
  message(paste(
    c(sprintf("%d of %d elements missing or blank:", smry$missing, smry$total),
      paste0("  ", smry$detail)),
    collapse = "\n"
  ))
  invisible(NULL)
}

#' @noRd
.show_missing_vector <- function(x) {
  smry <- .summarise_vector(x)
  if (smry$missing == 0) {
    message("✓ Complete data supplied.")
    return(invisible(NULL))
  }
  message(paste(
    c(sprintf("%d of %d elements missing:", smry$missing, smry$total),
      paste0("  ", smry$detail)),
    collapse = "\n"
  ))
  invisible(NULL)
}
