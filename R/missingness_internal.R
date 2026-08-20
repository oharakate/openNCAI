# Internal helpers shared by check_missing() and show_missing(). Not exported.

#' Test whether values in a vector count as "empty"
#'
#' A value is empty if it is `NA`, or - for character vectors - if it is
#' an empty string or contains only whitespace.
#'
#' @param x A vector.
#' @return A logical vector, `TRUE` where `x` is empty.
#' @noRd
.is_empty_value <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == ""
  } else {
    is.na(x)
  }
}

#' Format a proportion as a percentage string, stepping up precision so a
#' nonzero proportion never displays as "0.0"
#'
#' @param p A proportion between 0 and 1.
#' @return A character string.
#' @noRd
.format_pct <- function(p) {
  if (is.na(p) || p == 0) return("0.0")
  for (digits in 1:4) {
    s <- formatC(p * 100, format = "f", digits = digits)
    if (as.numeric(s) > 0) return(s)
  }
  "<0.0001"
}

#' Summarise missingness in a data frame, checked cell by cell
#'
#' @param df A data frame.
#' @param label_col Optional column name to use as the row label instead
#'   of `rownames(df)`.
#' @param top_n How many of the worst rows to include in `detail`.
#' @return A list with `total`, `missing`, `pct`, and `detail` (character
#'   vector of the worst rows, most-missing first).
#' @noRd
.summarise_df <- function(df, label_col = NULL, top_n = 3) {
  empty_mat <- as.matrix(as.data.frame(lapply(df, .is_empty_value)))
  labels <- if (!is.null(label_col)) as.character(df[[label_col]]) else rownames(df)

  pct_per_row <- rowSums(empty_mat) / ncol(df)
  total <- length(empty_mat)
  missing <- sum(empty_mat)

  ord <- order(pct_per_row, decreasing = TRUE)
  worst <- ord[pct_per_row[ord] > 0]
  shown <- utils::head(worst, top_n)

  detail <- if (length(shown) > 0) {
    sprintf("%s: %s%% missing", labels[shown], vapply(pct_per_row[shown], .format_pct, character(1)))
  } else {
    character(0)
  }
  remaining <- length(worst) - length(shown)
  if (remaining > 0) detail <- c(detail, sprintf("...and %d more", remaining))

  list(total = total, missing = missing, pct = if (total > 0) missing / total else 0, detail = detail)
}

#' Summarise missingness in a named list (label tree or weight tree),
#' up to two levels of nesting
#'
#' Blank or `NA` names count as missing alongside blank/`NA` values,
#' since openNCAI matches on these labels downstream.
#'
#' @param x A named list.
#' @param path Internal: the display path built up during recursion.
#' @return A list with `total`, `missing`, `pct`, and `detail` (character
#'   vector, one line per missing name/value).
#' @noRd
.summarise_tree <- function(x, path = NULL) {
  total <- 0L
  missing <- 0L
  detail <- character(0)

  nm <- names(x)
  if (is.null(nm)) nm <- rep("", length(x))

  for (i in seq_along(x)) {
    name_blank <- is.na(nm[i]) || trimws(nm[i]) == ""
    el_path <- if (name_blank) {
      if (is.null(path)) paste0("[[", i, "]]") else paste0(path, "[[", i, "]]")
    } else {
      if (is.null(path)) nm[i] else paste0(path, "$", nm[i])
    }

    total <- total + 1L
    if (name_blank) {
      missing <- missing + 1L
      detail <- c(detail, sprintf("%s: name is blank", el_path))
    }

    el <- x[[i]]
    if (is.list(el)) {
      sub <- .summarise_tree(el, path = el_path)
      total <- total + sub$total
      missing <- missing + sub$missing
      detail <- c(detail, sub$detail)
    } else {
      empt <- .is_empty_value(el)
      total <- total + length(el)
      missing <- missing + sum(empt)
      if (any(empt)) {
        detail <- c(detail, sprintf("%s[%d]: value missing", el_path, which(empt)))
      }
    }
  }

  list(total = total, missing = missing, pct = if (total > 0) missing / total else 0, detail = detail)
}

#' Summarise missingness in a bare (non-list, non-data.frame) vector
#'
#' @param x An atomic vector.
#' @return A list with `total`, `missing`, `pct`, and `detail`.
#' @noRd
.summarise_vector <- function(x) {
  empt <- .is_empty_value(x)
  total <- length(x)
  missing <- sum(empt)
  nm <- names(x)

  detail <- character(0)
  if (any(empt)) {
    idx <- which(empt)
    label <- if (!is.null(nm)) nm[idx] else as.character(idx)
    detail <- sprintf("[%s]: value missing", label)
  }

  list(total = total, missing = missing, pct = if (total > 0) missing / total else 0, detail = detail)
}

#' Summarise missingness in a named list of data frames (e.g.
#' `ci_relevance_matrices`), per element
#'
#' @param x A named list of data frames.
#' @return A list with `total`, `missing`, `pct`, and `sub` (a named list
#'   of per-element summaries from `.summarise_df()`).
#' @noRd
.summarise_list_of_df <- function(x) {
  subs <- lapply(x, .summarise_df)
  total <- sum(vapply(subs, function(s) s$total, numeric(1)))
  missing <- sum(vapply(subs, function(s) s$missing, numeric(1)))
  list(total = total, missing = missing, pct = if (total > 0) missing / total else 0, sub = subs)
}

#' Dispatch an object to the right missingness-summary method based on its
#' shape
#'
#' @param x The object to summarise.
#' @param label_col Optional, passed through to `.summarise_df()`.
#' @return A summary list; see the individual `.summarise_*()` helpers.
#' @noRd
.summarise_missing <- function(x, label_col = NULL) {
  if (is.data.frame(x)) {
    return(.summarise_df(x, label_col = label_col))
  }
  if (is.list(x)) {
    if (length(x) > 0 && all(vapply(x, is.data.frame, logical(1)))) {
      return(.summarise_list_of_df(x))
    }
    return(.summarise_tree(x))
  }
  if (is.atomic(x)) {
    return(.summarise_vector(x))
  }
  stop("Don't know how to check an object of class ", paste(class(x), collapse = "/"), " for missingness.")
}
