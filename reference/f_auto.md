# Automatic selection of predictor scoring method

Internal function to select a proper `f_...()` function to compute
preference order depending on the types of the response variable and the
predictors. The selection criteria is available as a dataframe generated
by
[`f_auto_rules()`](https://blasbenito.github.io/collinear/reference/f_auto_rules.md).

## Usage

``` r
f_auto(df = NULL, response = NULL, predictors = NULL, quiet = FALSE, ...)
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

- predictors:

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

function name

## See also

Other preference_order_tools:
[`f_auto_rules()`](https://blasbenito.github.io/collinear/reference/f_auto_rules.md),
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)

## Examples

``` r
data(
  vi_smol,
  vi_predictors_numeric,
  vi_predictors_categorical,
  vi_predictors
  )

f_auto(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric
  )
#> 
#> collinear::f_auto(): selected function 'f_numeric_glm()' to compute preference order.
#> [1] "f_numeric_glm"

f_auto(
  df = vi_smol,
  response = "vi_binomial",
  predictors = vi_predictors_numeric
  )
#> 
#> collinear::f_auto(): selected function 'f_binomial_glm()' to compute preference order.
#> [1] "f_binomial_glm"

f_auto(
  df = vi_smol,
  response = "vi_categorical",
  predictors = vi_predictors_categorical
  )
#> 
#> collinear::f_auto()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - vi_categorical
#>  - koppen_zone
#>  - koppen_group
#>  - koppen_description
#>  - biogeo_ecoregion
#>  - biogeo_biome
#>  - biogeo_realm
#>  - country_name
#>  - continent
#>  - region
#>  - subregion
#> 
#> collinear::f_auto(): selected function 'f_categorical_rf()' to compute preference order.
#> [1] "f_categorical_rf"

```
