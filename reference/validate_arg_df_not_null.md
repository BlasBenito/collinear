# Ensure that argument `df` is not `NULL`

Internal function to validate the default value of the argument `df`.

## Usage

``` r
validate_arg_df_not_null(df = NULL, function_name = NULL)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

## Value

dataframe

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
data(vi_smol)
df <- validate_arg_df_not_null(
  df = vi_smol
  )
```
