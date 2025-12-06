# Build hierarchical function names for messages

Concatenates parent and child function names for a better message,
warning, and error tracing.

## Usage

``` r
validate_arg_function_name(default_name = NULL, function_name = NULL, ...)
```

## Arguments

- default_name:

  (optional, character) Name of the calling function. Default: NULL

- function_name:

  (optional, character) Name of the parent function. Default: NULL

- ...:

  (optional) Used to pass `function_name` within these functions that
  don't have this argument.

## Value

character

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
x <- validate_arg_function_name(
  default_name = "child_function",
  function_name = "parent_function"
)

message(x)
#> parent_function
#> └── child_function
```
