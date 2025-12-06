# Check and validate argument `quiet`

Internal function to validate the logical argument `quiet`, which
triggers messaging when `FALSE`

## Usage

``` r
validate_arg_quiet(quiet = FALSE, function_name = NULL)
```

## Arguments

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

## Value

logical

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
x <- validate_arg_quiet(
  quiet = TRUE
  )

attributes(x)$validated
#> NULL
```
