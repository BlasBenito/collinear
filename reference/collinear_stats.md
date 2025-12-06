# Compute summary statistics for correlation and VIF

Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25,
median (0.5), 0.75, and 0.95 of the correlations and variance inflation
factors in a given dataframe. Wraps the functions
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md)
and
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Usage

``` r
collinear_stats(df = NULL, predictors = NULL, quiet = FALSE, ...)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with predictors or
  the output of
  [`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md).
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

dataframe with columns `method` (with values "correlation" and "vif"),
`statistic` and `value`

## See also

Other multicollinearity_assessment:
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md),
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md),
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Examples

``` r
data(
  vi_smol,
  vi_predictors_numeric
  )

## OPTIONAL: parallelization setup
## irrelevant when all predictors are numeric
## only worth it for large data with many categoricals
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

## OPTIONAL: progress bar
# progressr::handlers(global = TRUE)

x <- collinear_stats(
  df = vi_smol,
  predictors = vi_predictors_numeric
)

x
#>         method     statistic     value
#> 1  correlation             n 1081.0000
#> 2  correlation       minimum    0.0011
#> 3  correlation quantile_0.05    0.0426
#> 4  correlation quantile_0.25    0.1817
#> 5  correlation          mean    0.3980
#> 6  correlation        median    0.3610
#> 7  correlation quantile_0.75    0.6184
#> 8  correlation quantile_0.95    0.8264
#> 9  correlation       maximum    0.9893
#> 10         vif             n   47.0000
#> 11         vif       minimum    1.8158
#> 12         vif quantile_0.05    3.7499
#> 13         vif quantile_0.25   58.1814
#> 14         vif          mean  214.5167
#> 15         vif        median  170.1444
#> 16         vif quantile_0.75  354.2920
#> 17         vif quantile_0.95  520.9411
#> 18         vif       maximum  553.2944

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
