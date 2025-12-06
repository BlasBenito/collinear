# Check and constrain argument `max_vif`

Check and constrain argument `max_vif`

## Usage

``` r
validate_arg_max_vif(max_vif = NULL, quiet = FALSE, function_name = NULL)
```

## Arguments

- max_vif:

  (optional, numeric or NULL) Maximum Variance Inflation Factor allowed
  for `predictors` during multicollinearity filtering. Recommended
  values are between 2.5 (strict) and 10 (permissive). Default: 5

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
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
max_vif <- validate_arg_max_vif(
  max_vif = 11, #wrong value
  quiet = FALSE
)
#> 
#> collinear::validate_arg_max_vif(): argument 'max_vif' is outside its valid range (>=1 to <=10), resetting it to '5'.

max_vif
#> [1] 5
#> attr(,"validated")
#> [1] TRUE
attributes(max_vif)$validated
#> [1] TRUE
```
