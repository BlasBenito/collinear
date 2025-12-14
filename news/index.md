# Changelog

## collinear 3.0.0

CRAN release: 2025-12-08

### Breaking Changes

#### API Changes

- **Argument `response` renamed to `responses`**: Now accepts multiple
  response variables. Functions affected:
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
  [`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md),
  and related validation functions.

- **Argument `encoding_method` defaults to `NULL` in
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)**:
  Target encoding is now opt-in rather than automatic. Previously
  defaulted to `"mean"`.

- **Default values changed for `max_cor` and `max_vif`**: Both now
  default to `NULL`, triggering adaptive threshold computation based on
  the correlation structure of the data.

- **Output structure changed for
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)**:
  Now returns a list of class `collinear_output` containing sub-lists of
  class `collinear_selection`, each with `response`, `df`,
  `preference_order`, `selection`, and `formulas` slots. Previously
  returned a character vector or named list of character vectors.

#### Renamed Functions

| Old Name (v2.0)                       | New Name (v3.0)                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|---------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `identify_predictors()`               | Split into [`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md), [`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md), [`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md), [`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md) |
| `identify_predictors_categorical()`   | [`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md)                                                                                                                                                                                                                                                                                                                                                              |
| `identify_predictors_numeric()`       | [`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md)                                                                                                                                                                                                                                                                                                                                                                      |
| `identify_predictors_zero_variance()` | [`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)                                                                                                                                                                                                                                                                                                                                                          |
| `identify_predictors_type()`          | Removed (merged into [`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md))                                                                                                                                                                                                                                                                                                                                                    |

#### Renamed `f_` Functions for Preference Order

| Old Name (v2.0)        | New Name (v3.0)                                                                              |
|------------------------|----------------------------------------------------------------------------------------------|
| `f_r2_glm_gaussian()`  | [`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md)       |
| `f_r2_gam_gaussian()`  | [`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md)       |
| `f_r2_rf()`            | [`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md)         |
| `f_r2_glm_poisson()`   | [`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md)           |
| `f_r2_gam_poisson()`   | [`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md)           |
| `f_auc_glm_binomial()` | [`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md)     |
| `f_auc_gam_binomial()` | [`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md)     |
| `f_auc_rf_binomial()`  | [`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md)       |
| `f_v_rf()`             | [`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md) |
| —                      | [`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md) (new)       |

------------------------------------------------------------------------

### Major New Features

#### Adaptive Multicollinearity Thresholds

When both `max_cor = NULL` and `max_vif = NULL`, the function now
automatically determines optimal filtering thresholds using:

- The 75th percentile of pairwise correlations as input
- A sigmoid transformation that smoothly transitions between.
  conservative (VIF ≈ 2.5) and permissive (VIF ≈ 7.5) thresholds.
- A GAM model (`gam_cor_to_vif`) mapping correlation thresholds to
  equivalent VIF values.

This data-driven approach adapts to each dataset’s correlation
structure, preventing over-filtering while maintaining statistically
meaningful bounds.

#### Tidymodels Integration

- New
  [`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md):
  Recipe step for multicollinearity filtering in tidymodels workflows.
- Implements proper
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) and
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html) methods
  following recipes architecture.

#### Cross-Validation Support in Preference Order

- New arguments `cv_training_fraction` and `cv_iterations` in
  [`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
  and passed through
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).
- Enables robust predictor ranking through repeated train/test splits.

#### Rich Output Structure

[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
now returns comprehensive results including:

- Filtered dataframe with response and selected predictors.
- Preference order dataframe with rankings.
- Ready-to-use model formulas (linear, smooth/GAM, classification).

S3 methods [`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) for
`collinear_output` and `collinear_selection` classes provide clean
output formatting.

#### Correlation Matrix Improvements

- [`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md)
  now returns signed correlations, preserving the positive semi-definite
  property required for VIF calculations.
- Absolute values applied only when comparing against `max_cor`
  thresholds.
- Fixes numerical instability that could produce negative VIF scores.

------------------------------------------------------------------------

### New Functions

#### Multicollinearity Assessment

- [`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md):
  Compute summary statistics for both correlation and VIF.
- [`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md):
  Summary statistics for pairwise correlations.
- [`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md):
  Summary statistics for variance inflation factors.

#### Preference Order

- [`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md):
  Score integer count predictors with random forest.

#### S3 Methods

- [`print.collinear_output()`](https://blasbenito.github.io/collinear/reference/print.collinear_output.md)
- [`print.collinear_selection()`](https://blasbenito.github.io/collinear/reference/print.collinear_selection.md)
- [`summary.collinear_output()`](https://blasbenito.github.io/collinear/reference/summary.collinear_output.md)
- [`summary.collinear_selection()`](https://blasbenito.github.io/collinear/reference/summary.collinear_selection.md)

#### New Datasets and Models

| Name                             | Description                                                    |
|----------------------------------|----------------------------------------------------------------|
| `experiment_adaptive_thresholds` | Validation experiment results (10,000 iterations)              |
| `experiment_cor_vs_vif`          | Correlation vs VIF equivalence experiment results              |
| `gam_cor_to_vif`                 | Fitted GAM for mapping `max_cor` to `max_vif`                  |
| `prediction_cor_to_vif`          | Look-up table for threshold equivalence                        |
| `toy`                            | Simple dataset illustrating multicollinearity concepts         |
| `vi_smol`                        | Smaller version of `vi` dataset (610 rows) for faster examples |
| `vi_responses`                   | Character vector of response variable names                    |

------------------------------------------------------------------------

### Improvements

#### VIF Computation

- Ridge regularization fallback for near-singular matrices.
- Improved tolerance calculation for
  [`solve()`](https://rdrr.io/r/base/solve.html) to prevent false
  singularity detection.
- VIF values exceeding 1M are now capped to `Inf`.

#### Validation

- New `validate_arg_*()` functions provide consistent argument checking
  across the package.
- Hierarchical function name tracking for clearer error messages.

#### Documentation

- Comprehensive roxygen documentation with working examples.
- `@family` tags for better cross-referencing.
- `@inheritSection` for consistent documentation of shared concepts.

------------------------------------------------------------------------

### Bug Fixes

- Fixed correlation matrix handling that destroyed positive
  semi-definite property when applying
  [`abs()`](https://rdrr.io/r/base/MathFun.html) before VIF computation.
- Fixed edge cases in VIF computation for ill-conditioned matrices.
- Proper handling of single-predictor cases across all functions.

------------------------------------------------------------------------

### Deprecated

- Value `"auto"` in `preference_order` argument (ignored with message)

------------------------------------------------------------------------

## collinear 2.0.0

CRAN release: 2024-11-08

### Main Improvements

1.  **Expanded Functionality**: Functions
    [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
    and
    [`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
    support both categorical and numeric responses and predictors, and
    can handle several responses at once.

2.  **Robust Selection Algorithms**: Enhanced selection in
    [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
    and
    [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md).

3.  **Enhanced Functionality to Rank Predictors**: New functions to
    compute association between response and predictors covering most
    use-cases, and automated function selection depending on data
    features.

4.  **Simplified Target Encoding**: Streamlined and parallelized for
    better efficiency, and new default is `"loo"` (leave-one-out).

5.  **Parallelization and Progress Bars**: Utilizes `future` and
    `progressr` for enhanced performance and user experience.

------------------------------------------------------------------------

## collinear 1.1.1

CRAN release: 2023-12-08

- Initial CRAN release
- Basic multicollinearity filtering with
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
  [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
  and
  [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
- Target encoding methods: mean, rank, leave-one-out
- Preference order functionality
- Support for mixed numeric and categorical predictors
