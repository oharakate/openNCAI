
# 1. Try to find the root of the package regardless of where the test is called from
pkg_root <- tryCatch(
  rprojroot::find_package_root_file(),
  error = function(e) "."
)

# 2. Construct the absolute path to data-raw
local_path <- file.path(pkg_root, "data-raw", "ncai_corrected.xlsx")

# 3. Final Check and Assignment
if (file.exists(local_path)) {
  ns_sheets_path <- local_path
} else {
  # If we are here, the file isn't at the expected path
  # We set it to empty string so skip_if logic works
  ns_sheets_path <- ""
}

# Set `n` random cells of a data frame to NA, for testing check_missing()/
# show_missing(). Replaces the old R/introduce_nas.R, which broke whenever
# ncol(df) >= nrow(df) (e.g. on ns_ci_scores) and gave no control over how
# much missingness was introduced.
df_with_missing <- function(df, n = 1) {
  cells <- expand.grid(row = seq_len(nrow(df)), col = seq_len(ncol(df)))
  chosen <- cells[sample(nrow(cells), size = n), , drop = FALSE]
  for (i in seq_len(nrow(chosen))) {
    df[chosen$row[i], chosen$col[i]] <- NA
  }
  df
}

# Set `n` random elements of an atomic vector to NA.
vector_with_missing <- function(x, n = 1) {
  x[sample(seq_along(x), size = n)] <- NA
  x
}

