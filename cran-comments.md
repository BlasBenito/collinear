## Test environments

Tested via GitHub actions with usethis::use_github_action("check-standard"):

 + macos-latest (release)
 + ubuntu-latest (devel)
 + ubuntu-latest (oldrel-1)
 + ubuntu-latest (release)
 + windows-latest (release)

## R CMD check results

── R CMD check results ──────── collinear 1.0.0 ────
Duration: 2m 50.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Pre-release checks

**DESCRIPTION**

  + Proof read Title: and Description:                               YES
  + Title has title case:                                            YES
  + Title is not redundant:                                          YES
  + Software / package names are in quotes:                          YES
  + Description: is not a single sentence:                           YES
  + Only publication titles are double quoted:                       YES
  + Authors@R: includes a copyright holder (role 'cph'):             YES
  + Acronyms are fully expanded the first time they are mentioned:   N/A
  + There are references describing the methods:                     YES
  + References are correctly formatted:                              YES
  + License year is updated:                                         N/A 
  
**DOCUMENTATION**
  
  + Passed documentation through spellcheck:                        YES
  + All links are https rather than http:                           YES
  + Links to CRAN packages are canonical:                           YES
  + Relative links (file URIE) exist or are not broken:             YES
  + All exported functions have @returns and @examples              YES
  + The tag \dontrun is not used in any example:                    YES
  + Examples running for more than 5s are wrapped in donttest:      YES
  + There is no commented code in the @examples sections:           YES
  + Check for un-exported functions with roxygen examples:          DONE
  + Examples run on one thread:                                     YES


**CODE**

  + Check that no function modifies the user's options     DONE
