## Identified issue

This new release responds to the following email by Prof Brian Ripley:

  Dear maintainer,
  
  Please see the problems shown on
  <https://cran.r-project.org/web/checks/check_results_collinear.html>.
  
  Please correct before 2023-12-21 to safely retain your package on CRAN.
  
  Do remember to look at the 'Additional issues'.
  
  The CRAN Team

I can only guess that the received notice refers to the noLD check shown in the "Additional issues" section, which reads as follows:

* using log directory ‘/data/gannet/ripley/R/packages/tests-noLD/collinear.Rcheck’
* using R Under development (unstable) (2023-12-04 r85659)
* using platform: x86_64-pc-linux-gnu
* ...
* checking tests ...
  Running ‘spelling.R’
  Running ‘testthat.R’ [169s/170s]
 [170s/170s] ERROR
Running the tests in ‘tests/testthat.R’ failed.
Complete output:
  > # This file is part of the standard setup for testthat.
  > # It is recommended that you do not modify it.
  > #
  > # Where should you do additional test configuration?
  > # Learn more about the roles of various files in:
  > # * https://r-pkgs.org/tests.html
  > # * https://testthat.r-lib.org/reference/test_package.html#special-files
  > 
  > library(testthat)
  > library(collinear)
  > 
  > test_check("collinear")
  [ FAIL 1 | WARN 0 | SKIP 0 | PASS 120 ]
  
  ══ Failed tests ════════════════════════════════════════════════════════════════
  ── Error ('test-vif_df.R:37:3'): `vif_df()` works ──────────────────────────────
  Error in `value[[3L]](cond)`: the VIF computation failed. Please check for perfect correlations between predictors, or an excessive number of NA values in the 'df' argument.
  Backtrace:
      ▆
   1. └─collinear::vif_df(df = vi, response = "vi_mean", predictors = vi_predictors) at test-vif_df.R:37:3
   2.   └─base::tryCatch(...)
   3.     └─base (local) tryCatchList(expr, classes, parentenv, handlers)
   4.       └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
   5.         └─value[[3L]](cond)
  
  [ FAIL 1 | WARN 0 | SKIP 0 | PASS 120 ]
  Error: Test failures
  Execution halted
* checking PDF version of manual ... OK
* checking HTML version of manual ... OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 ERROR, 1 NOTE
See
  ‘/data/gannet/ripley/R/packages/tests-noLD/collinear.Rcheck/00check.log’
for details.

Command exited with non-zero status 1
Time 4:36.35, 249.72 + 10.45

## Solution

The the offensive code was in the line 116 of the function vif_df(), which read as follows:

```r 
vif.df <- cor.matrix |>
       solve(tol = 0) |>
       diag() |>
       ...
```
       
The new version reads:

```r 
vif.df <- cor.matrix |>
       solve() |>
       diag() |>
       ...
```

However, when tol != 0, solve() breaks when variables with perfect correlations are introduced. As such, all failing test and examples now ensure that no perfect correlations reach solve(), as they produce errors. Also, a warning has been added to the documentation of vif_df() and vif_select() to let the user now that perfect correlations may break these functions.     

## Testing changes in noLD

I tested these changes in the noLD platform provided by rhub:

```r
rhub::local_check_linux(".", image = "rhub/debian-gcc-devel-nold")

...

<R-hub local check results>
• image: rhub/debian-gcc-devel-nold
• output:
  R-hub Linux builder script v0.10.0 (c) R Consortium, 2018-2019
  
  Package: /tmp/RtmpLeiUIg/file3558a245af2e8/collinear_1.1.1.tar.gz
  Docker image: rhub/debian-gcc-devel-nold
  Env vars: 
  ...
• container_name: 3c762b6e-3759-4368-adfb-e71cad78f781-2
• artifacts: 
  /tmp/RtmpLeiUIg/file3558a12449344
• check_result:
── R CMD check results ────────────────────────────────────── collinear 1.1.1 ────
Duration: 0ms

❯ checking data for non-ASCII characters ... NOTE
    Note: found 89 marked Latin-1 strings
    Note: found 1203 marked UTF-8 strings

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

The only NOTE refers to Latin-1 and UTF-8 strings.


## R CMD check results

── R CMD check results ────────────── collinear 1.1.1 ────
Duration: 1m 15.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

### Tested platforms

  + macos-latest (release)
  + ubuntu-latest (devel)
  + ubuntu-latest (oldrel-1)
  + ubuntu-latest (release)
  + windows-latest (release)
  
## Test results

==> devtools::test()

ℹ Testing collinear
✔ | F W  S  OK | Context
✔ |          5 | auc_score                                  
✔ |          2 | case_weights                               
✔ |         16 | collinear [1.4s]                           
✔ |          6 | cor_df [9.8s]                              
✔ |          8 | cor_matrix [9.5s]                          
✔ |          4 | cor_select [18.7s]                         
✔ |          3 | cramer_v                                   
✔ |          6 | identify [1.2s]                            
✔ |         33 | preference_order [13.6s]                   
✔ |          9 | target_encoding_lab                        
✔ |         11 | target_encoding_methods                    
✔ |          7 | validate [1.2s]                            
✔ |          8 | vif_df                                     
✔ |          6 | vif_select                                 

══ Results ═════════════════════════════════════════════════
Duration: 59.1 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 124 ]
