# openNCAI (development version)

* New `calc_ncai_subset()` calculates the NCAI for any combination of
  habitats and ecosystem services, such as cultural services from woodland
  habitats. Habitats and services can be given as detailed labels or as group
  names from the label trees, and unknown labels are reported as an error.
* `index_and_smooth()` is now exported, for indexing custom subsets of the
  yearly NCAI matrices. It now gives a clear error when `year_one` is not one
  of the years, or when the total in `year_one` is zero.
* `get_ncai()` now passes `year_one` through to the `"by_broad_habitat"` and
  `"by_ecosystem_service_type"` breakdowns. Previously these were always
  indexed on the first year of `year_list`, whatever `year_one` was set to.
* `get_ncai()` now requires `total_indicator_relevances_constant >= 0`, and
  habitat/ecosystem service cells with no relevant condition indicators are
  always given a neutral flow of 100, including when the constant is 0.

# openNCAI 0.2.0

* Added `check_missing()` and `show_missing()` for checking and reporting
  missing data in pipeline inputs. `get_ncai()` now runs `check_missing()`
  on its core inputs before calculation and stops with a report naming any
  affected objects.
* Added Chris Littleboy as a package author.

# openNCAI 0.1.0

* Initial CRAN submission.
