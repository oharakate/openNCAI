# CLAUDE.md

Project-specific guidance for Claude Code when working on this R package.

## Package overview

- Package name: `openNCAI`
- Purpose: openNCAI calculates a Natural Capital Assets Index (NCAI) a
  time series of a single-figure variable which represents the changing 
  capacity of natural habitats to produce ecosystem services over time. 

## Development workflow

- Load the package for interactive testing: `devtools::load_all()`
- Run the full test suite: `devtools::test()`
- Run a single test file: `testthat::test_file("tests/testthat/test-<name>.R")`
- Update documentation after changing roxygen2 comments: `devtools::document()`
- Check the package before any release-related work: `devtools::check()`

## Testing

- Uses **testthat edition 3** (`Config/testthat/edition: 3` in DESCRIPTION).
- Use edition 3 idioms: `expect_snapshot()` for output/error/message checks,
  `expect_error(..., class = "some_condition")` for structured conditions,
  rather than matching on error message text.
- Every new exported function should have at least one test file covering
  the happy path and one meaningful edge case.
- Don't weaken or delete existing tests to make something pass — flag the
  conflict instead and ask before changing test expectations.

## Code style

- This package mixes tidyverse-style code (pipes, dplyr/purrr-style
  functions) and base R, depending on the file — match whatever style is
  already used in the file you're editing rather than converting it.
- Don't add roxygen2 `@importFrom` or new package dependencies without
  asking first, even if it would make something more idiomatic. Prefer a
  base R or already-imported-package solution.
- New exported functions need roxygen2 docs (`@param`, `@return`, `@export`,
  at least one `@examples` block).
- Use dataframes for input/output options as much as possible. 

## Things to avoid

- Check with me before changing existing working code. I am interested in 
  changes that will optimise or make my package more efficient. 
- **Do not reformat or restyle code that isn't part of the change you were
  asked to make.** No reflowing lines, no re-indenting, no swapping quote
  styles, no reordering arguments — even if it "looks cleaner." Minimal
  diffs only.
- **Do not add new dependencies** (Imports, Suggests, or otherwise) without
  explicitly asking and explaining why a base R / existing-dependency
  approach isn't sufficient.
- Don't add comments explaining *what* code does; only comment on *why*
  something non-obvious is done, and only if it isn't already clear from
  naming.
- Don't run `devtools::check()` or CRAN-submission-related commands
  automatically without being asked — these can be slow and noisy mid-task.

## Git

- Don't commit automatically. Stage and show a diff summary, then wait for
  confirmation before committing.
- Write commit messages in imperative mood, one-line summary + optional
  body, no attribution footers.

## Notes on this project

[Add any package-specific quirks here as you go — e.g. dependencies whose
docs aren't well represented in training data, unusual internal
conventions, or files that need special care. You can paste `btw::btw()`
output for such a dependency directly into this file.]
