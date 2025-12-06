# collinear 3.0.0

## Breaking Changes

### API Changes

- **Argument `response` renamed to `responses`**: Now accepts multiple response variables. Functions affected: `collinear()`, `preference_order()`, and related validation functions.

- **Argument `encoding_method` defaults to `NULL` in `collinear()`**: Target encoding is now opt-in rather than automatic. Previously defaulted to `"mean"`.

- **Default values changed for `max_cor` and `max_vif`**: Both now default to `NULL`, triggering adaptive threshold computation based on the correlation structure of the data.

- **Output structure changed for `collinear()`**: Now returns a list of class `collinear_output` containing sub-lists of class `collinear_selection`, each with `response`, `df`, `preference_order`, `selection`, and `formulas` slots. Previously returned a character vector or named list of character vectors.

### Renamed Functions

| Old Name (v2.0) | New Name (v3.0) |
|-----------------|-----------------|
| `identify_predictors()` | Split into `identify_valid_variables()`, `identify_numeric_variables()`, `identify_categorical_variables()`, `identify_logical_variables()` |
| `identify_predictors_categorical()` | `identify_categorical_variables()` |
| `identify_predictors_numeric()` | `identify_numeric_variables()` |
| `identify_predictors_zero_variance()` | `identify_zero_variance_variables()` |
| `identify_predictors_type()` | Removed (merged into `identify_valid_variables()`) |

### Renamed `f_` Functions for Preference Order

| Old Name (v2.0) | New Name (v3.0) |
|-----------------|-----------------|
| `f_r2_glm_gaussian()` | `f_numeric_glm()` |
| `f_r2_gam_gaussian()` | `f_numeric_gam()` |
| `f_r2_rf()` | `f_numeric_rf()` |
| `f_r2_glm_poisson()` | `f_count_glm()` |
| `f_r2_gam_poisson()` | `f_count_gam()` |
| `f_auc_glm_binomial()` | `f_binomial_glm()` |
| `f_auc_gam_binomial()` | `f_binomial_gam()` |
| `f_auc_rf_binomial()` | `f_binomial_rf()` |
| `f_v_rf()` | `f_categorical_rf()` |
| — | `f_count_rf()` (new) |

---

## Major New Features

### Adaptive Multicollinearity Thresholds

When both `max_cor = NULL` and `max_vif = NULL`, the function now automatically determines optimal filtering thresholds using:

- The 75th percentile of pairwise correlations as input
- A sigmoid transformation that smoothly transitions between. conservative (VIF ≈ 2.5) and permissive (VIF ≈ 7.5) thresholds.
- A GAM model (`gam_cor_to_vif`) mapping correlation thresholds to equivalent VIF values.

This data-driven approach adapts to each dataset's correlation structure, preventing over-filtering while maintaining statistically meaningful bounds.

### Tidymodels Integration

- New `step_collinear()`: Recipe step for multicollinearity filtering in tidymodels workflows.
- Implements proper `prep()` and `bake()` methods following recipes architecture.

### Cross-Validation Support in Preference Order

- New arguments `cv_training_fraction` and `cv_iterations` in `preference_order()` and passed through `collinear()`.
- Enables robust predictor ranking through repeated train/test splits.

### Rich Output Structure

`collinear()` now returns comprehensive results including:

- Filtered dataframe with response and selected predictors.
- Preference order dataframe with rankings.
- Ready-to-use model formulas (linear, smooth/GAM, classification).

S3 methods `print()` and `summary()` for `collinear_output` and `collinear_selection` classes provide clean output formatting.

### Correlation Matrix Improvements

- `cor_matrix()` now returns signed correlations, preserving the positive semi-definite property required for VIF calculations.
- Absolute values applied only when comparing against `max_cor` thresholds.
- Fixes numerical instability that could produce negative VIF scores.

---

## New Functions

### Multicollinearity Assessment

- `collinear_stats()`: Compute summary statistics for both correlation and VIF.
- `cor_stats()`: Summary statistics for pairwise correlations.
- `vif_stats()`: Summary statistics for variance inflation factors.

### Preference Order

- `f_count_rf()`: Score integer count predictors with random forest.

### S3 Methods

- `print.collinear_output()`
- `print.collinear_selection()`
- `summary.collinear_output()`
- `summary.collinear_selection()`

### New Datasets and Models

| Name | Description |
|------|-------------|
| `experiment_adaptive_thresholds` | Validation experiment results (10,000 iterations) |
| `experiment_cor_vs_vif` | Correlation vs VIF equivalence experiment results |
| `gam_cor_to_vif` | Fitted GAM for mapping `max_cor` to `max_vif` |
| `prediction_cor_to_vif` | Look-up table for threshold equivalence |
| `toy` | Simple dataset illustrating multicollinearity concepts |
| `vi_smol` | Smaller version of `vi` dataset (610 rows) for faster examples |
| `vi_responses` | Character vector of response variable names |

---

## Improvements

### VIF Computation

- Ridge regularization fallback for near-singular matrices.
- Improved tolerance calculation for `solve()` to prevent false singularity detection.
- VIF values exceeding 1M are now capped to `Inf`.

### Validation

- New `validate_arg_*()` functions provide consistent argument checking across the package.
- Hierarchical function name tracking for clearer error messages.

### Documentation

- Comprehensive roxygen documentation with working examples.
- `@family` tags for better cross-referencing.
- `@inheritSection` for consistent documentation of shared concepts.

---

## Bug Fixes

- Fixed correlation matrix handling that destroyed positive semi-definite property when applying `abs()` before VIF computation.
- Fixed edge cases in VIF computation for ill-conditioned matrices.
- Proper handling of single-predictor cases across all functions.

---

## Deprecated

- Value `"auto"` in `preference_order` argument (ignored with message)

---

# collinear 2.0.0

## Main Improvements

1. **Expanded Functionality**: Functions `collinear()` and `preference_order()` support both categorical and numeric responses and predictors, and can handle several responses at once.

2. **Robust Selection Algorithms**: Enhanced selection in `vif_select()` and `cor_select()`.

3. **Enhanced Functionality to Rank Predictors**: New functions to compute association between response and predictors covering most use-cases, and automated function selection depending on data features.

4. **Simplified Target Encoding**: Streamlined and parallelized for better efficiency, and new default is `"loo"` (leave-one-out).

5. **Parallelization and Progress Bars**: Utilizes `future` and `progressr` for enhanced performance and user experience.

---

# collinear 1.1.1

- Initial CRAN release
- Basic multicollinearity filtering with `collinear()`, `cor_select()`, and `vif_select()`
- Target encoding methods: mean, rank, leave-one-out
- Preference order functionality
- Support for mixed numeric and categorical predictors
