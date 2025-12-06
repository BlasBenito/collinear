# Check and validate argument `f`

Check and validate argument `f`

## Usage

``` r
validate_arg_f(f = NULL, f_name = NULL, function_name = NULL)
```

## Arguments

- f:

  (optional: function name) Unquoted function name without parenthesis
  (see
  [f_functions](https://blasbenito.github.io/collinear/reference/f_functions.md)).
  By default calls to
  [`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md),
  which selects a suitable function depending on the nature of the
  response and predictors. Set to NULL if `responses = NULL`. If NULL,
  predictors are ranked from lower to higher multicollinearity. Default:
  `f_auto`

- f_name:

  (optional, string) Name of the function `f`, as returned by
  `deparse(substitute(f))`. Default: NULL

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

## Value

function

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
x <- validate_arg_f(f = f_auto)
```
