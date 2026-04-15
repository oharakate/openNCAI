
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

