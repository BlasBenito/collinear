# Cross-Validation Implementation for `preference_order()` and `f_numeric_rf()`

## Overview

This document describes the implementation of cross-validation functionality in the `preference_order()` function and its associated scoring functions (e.g., `f_numeric_rf()`). The goal is to provide more robust preference scores by avoiding inflated metrics that can occur when training and testing on the same data.

## Problem Statement

The original implementation of scoring functions like `f_numeric_rf()` computes R-squared values by:
1. Fitting a model on all available data
2. Predicting on the same data used for training
3. Computing R-squared between observations and predictions

This approach can produce **inflated R-squared values** because the model is evaluated on data it has already seen during training.

## Solution

Implement optional cross-validation by:
1. Adding `cv_training_fraction`, `cv_iterations`, and `seed` parameters to `preference_order()`
2. Passing these parameters to scoring functions via the ellipsis (`...`)
3. Using a unified code path in scoring functions that handles both CV and non-CV modes:
   - **Non-CV mode**: When `cv_training_fraction = 1` (default), train and test on all data
   - **Cross-validation mode**: When `cv_training_fraction < 1`, perform repeated train/test splits

## Implementation Details

### 1. Modifications to `preference_order()`

#### Function Signature

Add three new parameters with sensible defaults:

```r
preference_order <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    f = f_auto,
    cv_training_fraction = 1,  # NEW: Default 1 = no CV (train and test on all data)
    cv_iterations = 1,          # NEW: Default 1 = single iteration (no CV)
    seed = 1,                   # NEW: Random seed for reproducibility
    warn_limit = NULL,
    quiet = FALSE,
    ...
){
```

**Design rationale for defaults:**
- `cv_training_fraction = 1`: By default, use all data for both training and testing (original behavior)
- `cv_iterations = 1`: Single iteration (no repeated splits)
- `seed = 1`: Ensures reproducibility by default
- Together, these defaults preserve backward compatibility while enabling CV when desired

#### Parameter Validation

Add validation logic after existing parameter checks:

```r
# Validate cv_training_fraction
if(!is.null(cv_training_fraction)) {
  if(!is.numeric(cv_training_fraction) || cv_training_fraction < 0.1 || cv_training_fraction > 1) {
    stop(
      "\n",
      function_name,
      ": 'cv_training_fraction' must be numeric between 0.1 and 1 (inclusive).",
      call. = FALSE
    )
  }
}

# Validate cv_iterations
if(!is.null(cv_iterations)) {
  if(!is.numeric(cv_iterations) || cv_iterations < 1 || cv_iterations != floor(cv_iterations)) {
    stop(
      "\n",
      function_name,
      ": 'cv_iterations' must be a positive integer.",
      call. = FALSE
    )
  }
}

# Validate and set seed
if(!is.null(seed)) {
  if(!is.numeric(seed) || seed != floor(seed)) {
    stop(
      "\n",
      function_name,
      ": 'seed' must be an integer.",
      call. = FALSE
    )
  }
  set.seed(seed)
}
```

**Note:** Unlike the original design, `cv_training_fraction` and `cv_iterations` do NOT need to be provided together since they have sensible defaults that preserve backward compatibility.

#### Passing Parameters to Scoring Functions

Modify the `future.apply::future_lapply()` call to pass CV parameters:

```r
preference$preference <- future.apply::future_lapply(
  X = preference$predictor,
  FUN = function(x){
    
    p()
    
    f.response(
      df = data.frame(
        y = df[[response]],
        x = df[[x]]
      ) |>
        stats::na.omit(),
      cv_training_fraction = cv_training_fraction,  # NEW
      cv_iterations = cv_iterations                  # NEW
    )
    
  },
  future.seed = TRUE
) |>
  unlist() |>
  suppressWarnings()
```

### 2. Modifications to `f_numeric_rf()`

#### Function Signature

Keep the signature clean with just `df` and `...`:

```r
f_numeric_rf <- function(
    df,
    ...
){
```

#### Extract CV Parameters from Ellipsis

At the beginning of the function, after `function_name` validation:

```r
function_name <- validate_arg_function_name(
  default_name = "collinear::f_numeric_rf()",
  ... = ...
)

# Capture cv_training_fraction and cv_iterations from ellipsis
dots <- list(...)
cv_training_fraction <- dots$cv_training_fraction
cv_iterations <- dots$cv_iterations

# Set defaults if not provided
if(is.null(cv_training_fraction)) cv_training_fraction <- 1
if(is.null(cv_iterations)) cv_iterations <- 1
```

#### Implement Unified CV/Non-CV Logic

Replace the single `tryCatch` block with a unified approach that handles both CV and non-CV modes:

```r
df <- stats::na.omit(object = df)

# Unified approach: both CV and non-CV
tryCatch(
  {
    r2_scores <- numeric(cv_iterations)
    
    for(i in 1:cv_iterations) {
      
      # Split data (or use all data if cv_training_fraction = 1)
      if(cv_training_fraction < 1) {
        n_train <- floor(nrow(df) * cv_training_fraction)
        train_indices <- sample(x = nrow(df), size = n_train, replace = FALSE)
        train_data <- df[train_indices, , drop = FALSE]
        test_data <- df[-train_indices, , drop = FALSE]
      } else {
        # No split: use all data for both training and testing
        train_data <- df
        test_data <- df
      }
      
      # Fit on training
      m <- ranger::ranger(
        formula = y ~ x,
        data = train_data,
        num.threads = 1,
        num.trees = 100,
        min.node.size = max(c(floor(nrow(train_data) * 0.05), 5)),
        seed = i
      )
      
      # Predict on testing
      p <- stats::predict(
        object = m,
        data = test_data
      )$predictions
      
      # Score
      r2_scores[i] <- score_r2(
        o = test_data[["y"]],
        p = p,
        function_name = function_name
      )
    }
    
    # Return mean R-squared (or single value if cv_iterations = 1)
    return(mean(r2_scores, na.rm = TRUE))
  },
  error = function(e) {
    stop(
      "\n",
      function_name,
      ": ", conditionMessage(e), 
      call. = FALSE
    )
  }
)
```

**Key design points:**
- When `cv_training_fraction = 1` (default): Both `train_data` and `test_data` point to the full dataset (no CV)
- When `cv_training_fraction < 1`: Data is split into training and testing sets
- When `cv_iterations = 1` (default): Single iteration (no repeated splits)
- When `cv_iterations > 1`: Multiple iterations with different random splits
- The loop always runs at least once, ensuring consistent behavior

## Usage Examples

### Without Cross-Validation (Default Behavior)

```r
# Uses all data for training and testing (cv_training_fraction = 1, cv_iterations = 1)
# This is the original behavior, fully backward compatible
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf
)
```

### With Cross-Validation

```r
# Uses 80% training / 20% testing split, repeated 10 times
# Default seed = 1 ensures reproducibility
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf,
  cv_training_fraction = 0.8,
  cv_iterations = 10
)

# Using a custom seed
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf,
  cv_training_fraction = 0.8,
  cv_iterations = 10,
  seed = 42
)

# Non-reproducible (random) behavior
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf,
  cv_training_fraction = 0.8,
  cv_iterations = 10,
  seed = NULL
)

# Only splitting data without repeated iterations
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf,
  cv_training_fraction = 0.8,
  cv_iterations = 1  # Single split
)
```

### With Parallelization and Progress Bar

```r
# Setup parallelization
future::plan(
  future::multisession,
  workers = 4
)

# Enable progress bar
progressr::handlers(global = TRUE)

# Run with cross-validation
x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_numeric_rf,
  cv_training_fraction = 0.8,
  cv_iterations = 10
)

# Reset to sequential processing
future::plan(future::sequential)
```

## Key Design Decisions

### 1. Seed Management for Reproducibility

The `seed` argument is added at the `preference_order()` level with a default value of `1`:

**Design rationale:**
- **Default reproducibility**: Setting `seed = 1` by default ensures that results are reproducible across R sessions without requiring users to remember to set a seed
- **User control**: Users can set their own seed value or set `seed = NULL` for non-reproducible behavior
- **Simplicity**: Seed is set once at the top level rather than passed through multiple function layers
- **Compatibility with parallelization**: Works well with `future.apply::future_lapply()`'s `future.seed = TRUE` parameter

**How it works:**
- When `seed` is provided, `set.seed(seed)` is called early in `preference_order()`
- This controls randomness in:
  - Data splitting during cross-validation
  - Random forest model fitting (via the `seed` parameter in `ranger::ranger()`)
  - Parallel worker initialization (when using `future`)
- Within CV iterations, each iteration uses `seed = i` in the ranger call to ensure different but reproducible splits

**Why not pass seed through ellipsis to f functions?**
- Would add unnecessary complexity
- Setting seed at the top level is sufficient for full reproducibility
- Keeps function signatures clean
- Avoids confusion about what seed controls what randomness

### 2. Default Parameters and Backward Compatibility

The new parameters have carefully chosen defaults:
- `cv_training_fraction = 1`: No train/test split (use all data)
- `cv_iterations = 1`: Single iteration (no repeated sampling)
- `seed = 1`: Reproducible by default

**Benefits:**
- **Full backward compatibility**: Existing code works without changes
- **No breaking changes**: Default behavior is identical to original implementation
- **Progressive enhancement**: Users can opt-in to CV when desired
- **No parameter coupling**: Unlike the original design, users don't need to provide both CV parameters together

### 3. Ellipsis Usage
CV parameters are passed through the ellipsis (`...`) to maintain flexibility. This allows:
- Clean function signatures
- Easy extension to other scoring functions
- Backward compatibility with existing code

### 4. Seed Management in CV Iterations
- **Non-CV mode**: Uses `seed = 1` for reproducibility
- **CV mode**: Uses `seed = i` (iteration number) to ensure:
  - Different random splits across iterations
  - Reproducibility when re-running with same parameters

### 4. Error Handling
Separate `tryCatch` blocks for CV and non-CV paths provide:
- Clear error messages indicating which mode failed
- Better debugging experience

### 5. Aggregation
CV results are aggregated using `mean(r2_scores, na.rm = TRUE)`:
- `mean()`: Provides central tendency across iterations
- `na.rm = TRUE`: Handles edge cases where some iterations might fail
- Could be extended to return other statistics (median, sd, etc.) if needed

## Implementation Considerations

### Edge Cases

1. **Small datasets**: With small `n` and high `cv_training_fraction`, test sets may be very small
2. **Failed iterations**: The `na.rm = TRUE` handles cases where some CV iterations fail, but we may want to track this
3. **Variance**: Consider storing/reporting CV variance alongside mean R-squared
4. **Minimum training fraction**: The 0.1 lower limit prevents degenerate training sets

### Performance

- CV significantly increases computation time (`cv_iterations` × number of predictors)
- Parallelization via `future` helps but applies at the predictor level, not the iteration level
- For large datasets or many predictors, consider:
  - Reducing `cv_iterations` 
  - Increasing `cv_training_fraction` (closer to 1)
  - Using more parallel workers Parallelization via `future` helps but applies at the predictor level, not the iteration level
- For large datasets or many predictors, consider reducing `iterations` or increasing `training_fraction`

### Extension to Other Scoring Functions

This pattern should be applied to all scoring functions in the `f_*` family:
- `f_numeric_glm()`
- `f_numeric_gam()`
- `f_binomial_glm()`
- `f_binomial_rf()`
- `f_categorical_rf()`
- etc.

Each function would:
1. Extract CV parameters from ellipsis
2. Implement conditional branching
3. Use appropriate scoring metrics (AUC for binomial, Cramer's V for categorical, etc.)

## Documentation Updates Needed

### roxygen2 Documentation for `preference_order()`

Add to parameter documentation:

```r
#' @param training_fraction (optional, numeric) Proportion of data to use for training in cross-validation (between 0 and 1, exclusive). Must be provided together with `iterations`. When NULL (default), the function uses all data for both training and testing. Example: 0.8 for 80% training / 20% testing split.
#' @param iterations (optional, integer) Number of cross-validation iterations to perform. Must be provided together with `training_fraction`. When NULL (default), the function uses all data for both training and testing. Example: 10 for 10 repeated random splits.
#' @param seed (optional, integer) Random seed for reproducibility. Default: 1. When provided, ensures that results are identical across multiple runs with the same parameters. Set to NULL for non-reproducible (random) behavior.
```

Add to details section:

```r
#' When `training_fraction` and `iterations` are provided, the function performs cross-validation to obtain more robust preference scores. For each predictor, the data is randomly split into training and testing sets `iterations` times. The model is fitted on the training set and evaluated on the testing set for each iteration. The final preference score is the mean of the test scores across all iterations. This approach helps prevent inflated metrics that can occur when evaluating models on the same data used for training.
```

### roxygen2 Documentation for `f_numeric_rf()`

Add to parameter documentation:

```r
#' @param ... Additional arguments passed from `preference_order()`, including:
#' \itemize{
#'   \item `training_fraction`: Proportion of data for training in cross-validation
#'   \item `iterations`: Number of cross-validation iterations
#' }
```

Update description:

```r
#' When `training_fraction` and `iterations` are passed via `...`, the function performs cross-validation by repeatedly splitting the data, fitting on training sets, and computing R-squared on testing sets. The final result is the mean R-squared across all iterations.
```

## Testing Checklist

- [ ] Test with `training_fraction = NULL, iterations = NULL` (original behavior)
- [ ] Test with valid CV parameters (e.g., `training_fraction = 0.8, iterations = 10`)
- [ ] Test with only one parameter provided (should error)
- [ ] Test with invalid `training_fraction` values (≤0, ≥1, non-numeric)
- [ ] Test with invalid `iterations` values (<1, non-integer, non-numeric)
- [ ] Test with invalid `seed` values (non-integer, non-numeric)
- [ ] Test with very small datasets (edge case)
- [ ] Test with multiple responses
- [ ] Test with parallelization enabled
- [ ] Compare CV vs non-CV R-squared values (CV should generally be lower)
- [ ] Verify reproducibility with same seed and parameters
- [ ] Verify reproducibility with default seed (`seed = 1`)
- [ ] Verify different results with different seeds
- [ ] Verify random results with `seed = NULL`
- [ ] Test that `seed = NULL` produces different results on consecutive runs

## Future Enhancements

1. **Return CV statistics**: Option to return not just mean but also SD, min, max of CV scores
2. **Stratified sampling**: For categorical responses, ensure balanced splits
3. **K-fold CV**: Add option for standard k-fold instead of repeated random splits
4. **Early stopping**: Skip remaining iterations if variance is very low
5. **Parallel CV iterations**: Parallelize within iterations, not just across predictors
6. **Custom aggregation**: Allow users to specify aggregation function (mean, median, etc.)

## References

- Hastie, T., Tibshirani, R., & Friedman, J. (2009). *The Elements of Statistical Learning*. Chapter 7: Model Assessment and Selection.
- Kuhn, M., & Johnson, K. (2013). *Applied Predictive Modeling*. Chapter 4: Over-Fitting and Model Tuning.

---

**Document Version**: 1.0  
**Date**: October 15, 2025  
**Author**: Development Notes