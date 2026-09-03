## Resubmission - 02-09-2026 ##

This is a resubmission for version 0.2.0, a minor update to the previously
accepted version 0.1.0.

Changes in this version:

* Added `check_missing()` and `show_missing()`, exported functions for
  checking and reporting missing data in pipeline inputs. `get_ncai()` now
  runs `check_missing()` on its core inputs before calculation.
* Added Chris Littleboy as a package author (`Authors@R`).
* Updated two dead reference URLs in the vignettes (one site had been
  retired, one was already a mismatched link) to their correct, live
  replacements.

Test platforms
Local macOS (R 4.5.3)
win-builder (R-release, R-devel, R-oldrelease)
R-hub v2 (Linux, Windows, macOS; R-devel)

R CMD check results
0 errors | 0 warnings | 1 note

Spurious Notes
"unable to verify current time": This NOTE is produced by R CMD check's
network time-check step and reflects a local inability to reach the
time-verification service, not an issue with the package itself. No action
required.

"Found the following (possibly) invalid URLs: https://seea.un.org/en ...
Status: 403 Forbidden" (seen intermittently, not on every platform/run): the
URL is live and reachable by ordinary HTTP clients (verified directly). The
target site blocks requests whose User-Agent header contains the substring
"curl", which some R HTTP backends send as a default User-Agent when no
custom User-Agent is set; other platforms/R versions send a different
default User-Agent and pass. This is a false positive from the target
site's bot-mitigation rules interacting with the checking machine's HTTP
client identification, not a broken link.

"Found the following (possibly) invalid URLs:
https://www.nature.scot/professional-advice/social-and-economic-benefits-nature/natural-capital/scotlands-natural-capital-asset-index
Status: 403 Forbidden": this page is live and renders normally in an
ordinary web browser. The nature.scot domain serves a Cloudflare
JavaScript challenge ("Just a moment...") for this page to any HTTP client
that cannot execute JavaScript, which includes all automated URL checkers;
verified this returns 403 regardless of User-Agent (browser-identical,
plain curl, and R-style User-Agents all blocked identically). This is a
false positive from the target site's bot-mitigation, not a broken link;
note the other nature.scot URL referenced in DESCRIPTION (a static PDF) is
not affected, as the challenge appears to apply only to the site's HTML
page routes.

---------------

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
