# Removes `geometry` Column From `sf` Dataframes

Remove geometry column from sf objects

## Usage

``` r
drop_geometry_column(df = NULL, quiet = FALSE, ...)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

dataframe

## See also

Other argument_validation:
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol)

#creating fake geometry column without sf loaded
vi_smol$geometry <- NA
attr(
  x = vi_smol,
  which = "sf_column"
  ) <- "geometry"

#check new attribute
attributes(vi_smol)$sf_column
#> [1] "geometry"

#drop geometry column
df <- drop_geometry_column(
  df = vi_smol
  )
#> 
#> collinear::drop_geometry_column(): dropping geometry column from 'df'.

#checking that the geometry was droppped
"geometry" %in% colnames(df)
#> [1] FALSE
attributes(df)$sf_column
#> NULL
```
