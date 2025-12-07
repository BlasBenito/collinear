## Test environments
* macOS 14 (local), R 4.4.1
* Ubuntu 22.04 (GitHub Actions), R devel, release, oldrel
* Windows Server 2022 (GitHub Actions), R release
* win-builder (devel and release)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## win-builder (R-devel) results
Status: 2 NOTEs

1. **Possibly misspelled words in DESCRIPTION**
   * "VIF" appears in several locations.  
     This is an established statistical abbreviation (Variance Inflation Factor).

The NOTE regarding “invalid file URIs” in the README is a known false positive on win-devel when links refer to pkgdown-generated pages. These links resolve correctly on CRAN and are accepted in other platforms (including Linux and macOS). No other check reports them.

## rhub results
All platforms passed except:

* **Linux (r-devel)**: 1 ERROR  
  - `ERROR: compilation failed for package 'yaml'`  
  - This originates from the system toolchain on that rhub platform and is unrelated to the `collinear` package.

All other tested platforms (macOS x86_64, macOS arm64, Windows, Linux GCC/Clang variants, ASAN, MKL, noLD, noSuggests, atlas, intel, gcc13/14, clang16–19, ubuntu-gcc/clang, ubuntu-release, ubuntu-next) completed without errors or warnings.

## Downstream dependencies
No reverse dependencies.

## Major changes in 3.0.0
This is a major release with substantial API updates and new functionality.

### Breaking changes
* `response` renamed to `responses` (multi-response support).
* Target encoding defaults to `NULL`.
* `max_cor` and `max_vif` now default to `NULL` (adaptive thresholds).
* `collinear()` now returns structured `collinear_output` and `collinear_selection` objects.
* Several `identify_*` and `f_*` functions renamed.

### New features
* Adaptive multicollinearity thresholds.
* New tidymodels recipe step `step_collinear()`.
* Cross-validation support in `preference_order()`.
* New helper functions for correlation/VIF summaries.
* New S3 classes and methods.
* Additional datasets (GAM model, lookup tables, experiments).

### Improvements
* More stable VIF computation with regularization fallback.
* Preservation of signed correlation matrices for PSD stability.
* Improved argument validation and error messages.
* Reorganized, expanded documentation.

### Bug fixes
* Fixed PSD issues in correlation matrices affecting VIF.
* Fixed VIF edge cases under ill-conditioned matrices.
* Correct handling of single-predictor inputs.

## Additional notes
* All tidymodels-related features are properly guarded behind Suggests.
* Examples run quickly with no long-running computations.
