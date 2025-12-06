# Find near-zero variance variables in a dataframe

Returns the names of near-zero variance variables in a modelling
dataframe.

## Usage

``` r
identify_zero_variance_variables(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  decimals = 4,
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

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- decimals:

  (required, integer) Number of decimal places for the zero variance
  test. Smaller numbers will increase the number of variables detected
  as near-zero variance. Recommended values will depend on the range of
  the numeric variables in 'df'. Default: 4

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

character vector: names of near-zero variance columns.

## See also

Other data_types:
[`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md),
[`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md),
[`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md),
[`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md),
[`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol, vi_predictors)

#create zero and near variance predictors
vi_smol$zero_variance <- 1
vi_smol$near_zero_variance <- runif(
  n = nrow(vi_smol),
  min = 0,
  max = 0.0001
  )


#add to vi predictors
vi_predictors <- c(
  vi_predictors,
  "zero_variance",
  "near_zero_variance"
)

#identify zero variance predictors
x <- identify_zero_variance_variables(
  df = vi_smol,
  predictors = vi_predictors
)
#> 
#> collinear::identify_zero_variance_variables(): invalid predictors due to near-zero variance:
#>  - zero_variance
#>  - near_zero_variance

x
#> [1] "zero_variance"      "near_zero_variance"
```
