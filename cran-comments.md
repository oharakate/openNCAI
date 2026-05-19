## Resubmission - 08-05-2026 ##

This is a resubmission following failed incoming checks of submission on 
07-05-2026, addressing the following points:

1. Possibly mispelled words in description

  Words are names and domain-specific terms. Added the words to 
  WORDLIST.
  
2. Possibly invalid URL

  Linked website appears to be down and is still down at time of resubmission.
  Replaced the url with another relevant and confirmed url.
  
3. Incorrectly formatted DOI in description. 

  Corrected to use format <doi:prefix/suffix>
  

---------------

## Resubmission - 07-05-2026 ##

Thank you for the reviewer's comments. This is a resubmission addressing the 
following points:

1. Package/Software names in quotes
  
  Removed quotes from non-software terms (e.g. Scotland, NatureScot).


2. Description field spacing
  
  Stripped line breaks from description field. Checked for extraneous spaces. 


3. References format
  
  Updated the methodology reference in the DESCRIPTION file to the requested 
  format: authors (year) <URL>.


4. Use of the ::: operator

  Removed examples from the documentation of unexported helper functions.

  Removed all instances of ::: from the test suite. The package now uses only 
  :: or internal namespace calls.

5. \dontrun vs \donttest

  Replaced \dontrun{} with \donttest{} for examples that involve writing/reading 
  Excel files or have an execution time potentially exceeding 5 seconds. 



-----------


## Original submission - 04-05-2026

Resubmission Notes
This is a resubmission. The previous submission failed during the automated incoming checks because of syntax errors in the vignettes:

Fixed a character escape error (\O) in replicating_scotlands_ncai.Rmd.

Corrected YAML header formatting (stray colon and indentation) in openNCAI_in_brief.Rmd.

Verified that all vignettes now build successfully on local and R-hub platforms.

Test platforms
Local macOS (R 4.4.1)

R-hub v2 (Ubuntu Linux, R-release, GCC)

R-hub v2 (Fedora Linux, R-devel, clang)

win-builder (R-devel and R-release)

R CMD check results
0 errors | 0 warnings | 1 note

This is a new submission.

Notes on R-hub v2
Several R-devel platforms on R-hub v2 initially reported installation failures (Exit status: 1) during the lazy-loading phase. Investigation of the logs revealed an upstream environment issue: .onLoad failed in loadNamespace() for 'vctrs' with the error symbol bindings not supported yet.

This was identified as a platform-specific binary mismatch on the R-hub runners rather than a package-level error, as evidenced by:

Successful local checks (0 errors, 0 warnings, 0 notes).

Successful builds on R-release platforms.

Successful rebuilding of all vignettes in the most recent R-hub runs.

Spurious Notes
"Possibly misspelled words": The terms 'NCAI', 'NatureScot', and 'openNCAI' are domain-specific. These have been added to the inst/WORDLIST file to prevent future flags.

"Non-standard files/directories": The .github/ directory is used for R-hub v2 / GitHub Actions CI; the WORDLIST and cran-comments.md files are used for the CRAN submission process.
