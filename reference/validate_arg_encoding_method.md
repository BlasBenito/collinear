# Check and validate argument `encoding_method`

Internal function to validate the argument `encoding_method` of
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md).

## Usage

``` r
validate_arg_encoding_method(
  encoding_method = "loo",
  overwrite = NULL,
  quiet = FALSE,
  function_name = NULL
)
```

## Arguments

- encoding_method:

  (optional; character vector or NULL). Name of the target encoding
  methods. One or several of: "mean", "rank", "loo". If NULL, target
  encoding is ignored, and `df` is returned with no modification.
  Default: "loo"

- overwrite:

  (optional; logical) If TRUE, the original predictors in `df` are
  overwritten with their encoded versions, but only one encoding method,
  smoothing, white noise, and seed are allowed. Otherwise, encoded
  predictors with their descriptive names are added to `df`. Default:
  FALSE

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

## Value

character

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
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
x <- validate_arg_encoding_method(
  encoding_method = "wrong_method"
  )
#> 
#> collinear::validate_arg_encoding_method(): argument 'encoding_method' is not valid, resetting it to 'loo'.
```
