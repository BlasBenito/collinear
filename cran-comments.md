# Version 1.1.2

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
    + "rchk"
    + "ubuntu-clang"
    + "ubuntu-gcc12"
    + "ubuntu-next"
    + "ubuntu-release"
    + "valgrind"

## Local check

── R CMD check results ─────────── collinear 1.1.2 ────
Duration: 1m 41.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Local test

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


## Platform Checks

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
  "rchk",
  "ubuntu-clang",
  "ubuntu-gcc12",
  "ubuntu-next", 
  "ubuntu-release", 
  "valgrind"
  )
)
```

Job output: [https://github.com/BlasBenito/collinear/actions/runs/10944160248](https://github.com/BlasBenito/collinear/actions/runs/10944160248)


