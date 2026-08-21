#' Check pipeline inputs for missing data
#'
#' @description
#' Checks one or more openNCAI pipeline objects for missing data and
#' reports a summary via \code{message()}. Data frames are checked cell
#' by cell. Named lists (label trees and weight trees, up to two levels
#' of nesting, e.g. \code{habitats_label_tree} or
#' \code{within_importance_scores}) are checked element by element, and
#' blank or \code{NA} \emph{names} are treated as missing alongside blank
#' or \code{NA} \emph{values}, since openNCAI matches on these labels
#' downstream. Bare vectors (e.g. \code{year_list}) are checked element
#' by element. \code{ci_relevance_matrices}-shaped input (a named list of
#' data frames) is checked per element, with its own header line.
#'
#' \code{check_missing()} is run automatically at the start of
#' \code{\link{get_ncai}}; if any input has missing data, it stops the
#' pipeline before any calculation runs. It can also be called directly
#' on any named list of objects.
#'
#' @param inputs A named list of objects to check.
#' @param label_cols Optional named character vector mapping names in
#'   \code{inputs} to the column that should be used as the row label in
#'   the report, for data frames whose row names aren't meaningful (e.g.
#'   \code{c(indicator_directory = "ci_id")}).
#'
#' @return Invisibly, \code{TRUE} if no missing data was found. Stops
#'   with an error if any input has missing data.
#' @export
#'
#' @examples
#' check_missing(list(habitat_extent = ns_habitat_extent))
check_missing <- function(inputs, label_cols = NULL) {
  if (!is.list(inputs) || is.data.frame(inputs) || is.null(names(inputs)) || any(names(inputs) == "")) {
    stop("`inputs` must be a fully named list.")
  }

  object_names <- names(inputs)
  pad_width <- max(nchar(object_names))

  lines <- character(0)
  failing <- character(0)

  for (nm in object_names) {
    x <- inputs[[nm]]
    label_col <- if (!is.null(label_cols) && nm %in% names(label_cols)) label_cols[[nm]] else NULL
    smry <- .summarise_missing(x, label_col = label_col)

    if (!is.null(smry$sub)) {
      # A named list of data frames (e.g. ci_relevance_matrices). If every
      # element is complete, collapse to one line rather than one line per
      # element (which is otherwise all "Complete." noise); only expand to
      # per-element sub-lines when something is actually missing.
      if (smry$missing == 0) {
        header <- sprintf("%-*s", pad_width, nm)
        lines <- c(lines, sprintf("%s \u2713 Complete. (%d elements)", header, length(smry$sub)))
        next
      }

      lines <- c(lines, paste0(nm, ":"))
      sub_names <- names(smry$sub)
      sub_pad <- floor(stats::median(nchar(sub_names)))
      for (snm in sub_names) {
        ssmry <- smry$sub[[snm]]
        if (ssmry$missing == 0) {
          value <- "\u2713 Complete."
        } else {
          value <- paste0(.format_pct(ssmry$pct), "% missing")
          failing <- c(failing, paste0(nm, "$", snm))
        }
        entry <- if (nchar(snm) <= sub_pad) {
          sprintf("  %-*s %s", sub_pad, snm, value)
        } else {
          sprintf("  %s: %s", snm, value)
        }
        lines <- c(lines, entry)
      }
      next
    }

    header <- sprintf("%-*s", pad_width, nm)
    if (smry$missing == 0) {
      lines <- c(lines, paste0(header, " \u2713 Complete."))
    } else {
      lines <- c(lines, paste0(header, " ", .format_pct(smry$pct), "% missing"))
      for (d in smry$detail) lines <- c(lines, paste0("  ", d))
      failing <- c(failing, nm)
    }
  }

  message(paste(c("openNCAI missingness check:", lines), collapse = "\n"))

  if (length(failing) > 0) {
    stop(
      "openNCAI requires complete data for all inputs.\n\n",
      "Objects with missing values:\n",
      paste0("  - ", failing, collapse = "\n"), "\n\n",
      "See above for details and run show_missing() on a flagged object\n",
      "for more info.\n",
      "You may find openNCAI's template helpers \n",
      "create_ncai_template() and read_ncai_template() helpful for \n",
      "producing complete input data.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
