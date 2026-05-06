## New submission

This is a patch release (v3.0.1) addressing four bug fixes and moving
bundled example datasets to a companion package (`spatialData`).

### Bug fixes in this release

- `preference_order()`: namespace-qualified function names now matched correctly (#16)
- `model_formula()`: no longer crashes on `sf` spatial data frames (#17)
- `preference_order()`: NA values in the response variable handled correctly (#18)
- `score_auc()`: NA values in observations or predictions handled correctly (#19)

## Test environments

* macOS 14 (local), R 4.4.x
* Ubuntu 22.04 (GitHub Actions), R devel, release, oldrel-1
* Windows Server 2022 (GitHub Actions), R release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes
