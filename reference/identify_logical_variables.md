# Find logical variables in a dataframe

Identifies logical predictors and excludes those with constant values.

## Usage

``` r
identify_logical_variables(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- responses:

  (optional; character, character vector, or NULL) Name of one or
  several response variables in `df`. Default: NULL.

- predictors:

  (required, character vector) Names of the predictors to identify.
  Default: NULL

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

list:

- `valid`: character vector with valid logical predictor names.

- `invalid`: character vector with invalid logical predictor names.

## See also

Other data_types:
[`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md),
[`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md),
[`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md),
[`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md),
[`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol, vi_predictors)

#invalid logical
vi_smol$logical_invalid <- TRUE

#valid logical
vi_smol$logical_valid <- sample(
  x = c(TRUE, FALSE),
  size = nrow(vi_smol),
  replace = TRUE
)

x <- identify_logical_variables(
  df = vi_smol,
  predictors = c(
    vi_predictors,
    "logical_invalid",
    "logical_valid"
  )
)
#> 
#> collinear::identify_logical_variables(): invalid logical predictors due to constant values:
#>  - logical_invalid

x$valid
#> [1] "logical_valid"
x$invalid
#> [1] "logical_invalid"
```
