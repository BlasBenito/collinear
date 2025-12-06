# Detect response variable type for model selection

Used by
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md)
to identify the type of a response variable and select a proper
modelling method to compute preference order. Supported types are:

- "continuous-binary": decimal numbers and two unique values; results in
  a warning, as this type is difficult to model.

- "continuous-low": decimal numbers and 3 to 5 unique values; results in
  a message, as this type is difficult to model.

- "continuous-high": decimal numbers and more than 5 unique values.

- "integer-binomial": integer with 0s and 1s, suitable for binomial
  models.

- "integer-binary": integer with 2 unique values other than 0 and 1;
  returns a warning, as this type is difficult to model.

- "integer-low": integer with 3 to 5 unique values or meets specified
  thresholds.

- "integer-high": integer with more than 5 unique values suitable for
  count modelling.

- "categorical": character or factor with 2 or more levels.

- "unknown": when the response type cannot be determined.

## Usage

``` r
identify_response_type(df = NULL, response = NULL, quiet = FALSE, ...)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- response:

  (optional, character string) Name of a response variable in `df`.
  Default: NULL.

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

character string: response type

## See also

Other data_types:
[`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md),
[`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md),
[`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md),
[`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md),
[`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)

## Examples

``` r
data(vi_smol)

identify_response_type(
  df = vi_smol,
  response = "vi_numeric"
)
#> [1] "continuous-high"

identify_response_type(
  df = vi_smol,
  response = "vi_counts"
)
#> [1] "integer-high"

identify_response_type(
  df = vi_smol,
  response = "vi_binomial"
)
#> [1] "integer-binomial"

identify_response_type(
  df = vi_smol,
  response = "vi_categorical"
)
#> 
#> collinear::identify_response_type()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - vi_categorical
#> [1] "categorical"

identify_response_type(
  df = vi_smol,
  response = "vi_factor"
)
#> [1] "categorical"
```
