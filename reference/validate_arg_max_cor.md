# Check and constrain argument `max_cor`

Check and constrain argument `max_cor`

## Usage

``` r
validate_arg_max_cor(max_cor = NULL, quiet = FALSE, function_name = NULL)
```

## Arguments

- max_cor:

  (optional; numeric or NULL) Maximum correlation allowed between pairs
  of `predictors`. Valid values are between 0.01 and 0.99, and
  recommended values are between 0.5 (strict) and 0.9 (permissive).
  Default: 0.7

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

## Value

numeric or NULL

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
x <- validate_arg_max_cor(
  max_cor = 1.5, #wrong value
  quiet = FALSE
)
#> 
#> collinear::validate_arg_max_cor(): argument 'max_cor' is outside its valid range (>=0.1 to <=1), resetting it to '0.7'.

x
#> [1] 0.7
#> attr(,"validated")
#> [1] TRUE
attributes(x)$validated
#> [1] TRUE
```
