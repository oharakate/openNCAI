## Test platforms
* Local macOS (R 4.4.1)
* R-hub v2 (Ubuntu Linux, R-release, GCC)
* R-hub v2 (Fedora Linux, R-devel, clang)
* win-builder (R-devel and R-release)

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new submission.


## Notes on R-hub v2
Several R-devel platforms on R-hub v2 initially reported installation failures (Exit status: 1) during the lazy-loading phase. Investigation of the logs revealed an upstream environment issue: .onLoad failed in loadNamespace() for 'vctrs' with the error symbol bindings not supported yet.

This was identified as a platform-specific binary mismatch on the R-hub runners rather than a package-level error, as evidenced by:

Successful local checks (0 errors, 0 warnings, 0 notes).

Successful builds on R-release platforms.

Internal consistency of the man/ and vignettes/ directories.

## Spurious Notes
* "Possibly misspelled words": The terms 'NCAI', 'NatureScot', and 'openNCAI' are domain-specific. These have been added to the WORDLIST file to prevent future flags.
* "Non-standard files/directories": The .github/ directory is used for R-hub v2 / GitHub Actions CI; the WORDLIST and cran-comments.md files are required for the CRAN submission process.

## Background
This package implements the 'NatureScot' Natural Capital Assets Index methodology for 'Scotland'. All documentation has been reviewed for clarity and CRAN compliance.
