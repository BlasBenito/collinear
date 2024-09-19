## Version 1.1.2

The functions cor_select(), vif_select() and vif_df() now skip the analysis if only one predictor is available. This may happen in collinear() with highly correlated datasets, when cor_select() only returns one predictor and sends it to vif_select().

All warnings in all data validation functions are now messages to ensure they are printed in the correct order.

Function vif_df() now has the internal function vif_f() to compute the vif data frame, and this function is applied twice, once without modifying the correlation matrix, and if this fails, again by replacing 1 and -1 with 0.999 and -0.999 in the correlation matrix to try overcome the "singular matrix" issue.

The function validate_df() now takes into account the number of predictors as reference, along with min_rows, to warn the user about potential issues in the multicollinearity analysis due to the data frame dimensions.

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

── R CMD check results ─────────── collinear 1.1.2 ────
Duration: 1m 41.3s

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
✔ |         16 | collinear [2.6s]                      
✔ |          6 | cor_df [1.4s]                         
✔ |          8 | cor_matrix [1.4s]                     
✔ |          4 | cor_select [1.9s]                     
✔ |          3 | cramer_v                              
✔ |          6 | identify [3.7s]                       
✔ |         33 | preference_order [29.3s]              
✔ |          9 | target_encoding_lab [1.3s]            
✔ |         11 | target_encoding_methods [1.3s]        
✔ |          7 | validate [8.2s]                       
✔ |          8 | vif_df [1.4s]                         
✔ |          6 | vif_select [1.3s]                     

══ Results ════════════════════════════════════════════
Duration: 56.7 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 124 ]
