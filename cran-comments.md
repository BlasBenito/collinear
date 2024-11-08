# Version 2.0.0

  + Local check and tests performed in Ubuntu 20.04.6 LTS (Focal) on R 4.4.1: 0 errors, warnings, and notes.
  + Platform checks performed with rhub::rhub_check() for all available setups:
    + "linux"
    + "macos"
    + "macos-arm64"
    + "windows"
    + "atlas"
    + "c23"
    + "clang-asan"
    + "clang16"
    + "clang17"
    + "clang18"
    + "clang19"
    + "gcc13"
    + "gcc14"
    + "intel"
    + "mkl"
    + "nold"
    + "nosuggests"
    + "ubuntu-clang"
    + "ubuntu-gcc12"
    + "ubuntu-next"
    + "ubuntu-release"
    + "valgrind"

## Local check

── R CMD check results ── collinear 2.0.0 ────
Duration: 3m 19.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Local test

==> devtools::test()

ℹ Testing collinear
✔ | F W  S  OK | Context
✔ |          5 | auc [2.3s]                             
✔ |          3 | case_weights [1.8s]                    
✔ |         35 | collinear [56.6s]                      
✔ |          5 | cor_clusters [3.0s]                    
✔ |         15 | cor_df [5.5s]                          
✔ |          4 | cor_matrix [7.3s]                      
✔ |         23 | cor_select [16.6s]                     
✔ |          3 | cramer_v [1.2s]                        
✔ |          8 | identify [4.6s]                        
✔ |         17 | preference_order_methods [3.3s]        
✔ |         28 | preference_order [7.0s]                
✔ |          7 | target_encoding_lab [1.1s]             
✔ |          7 | target_encoding_methods                
✔ |          7 | validate [12.2s]                       
✔ |         18 | vif_df                                 
✔ |         23 | vif_select                             

══ Results ═════════════════════════════════════════════
Duration: 124.0 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 208 ]


## Platform Checks

[Link to GitHub Action](https://github.com/BlasBenito/collinear/actions/runs/11738731004)

```r
rhub::rhub_check(
  platforms = c(
  "linux", 
  "macos", 
  "macos-arm64",
  "windows", 
  "atlas",
  "c23",
  "clang-asan",
  "clang16",
  "clang17",
  "clang18",
  "clang19",
  "gcc13",
  "gcc14",
  "intel",
  "mkl",
  "nold",
  "nosuggests",
  "ubuntu-clang",
  "ubuntu-gcc12",
  "ubuntu-next", 
  "ubuntu-release"
  )
)
```




