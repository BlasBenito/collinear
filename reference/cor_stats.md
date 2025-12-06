# Compute summary statistics for absolute pairwise correlations

Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25,
median (0.5), 0.75, and 0.95 on the absolute values of the column
"correlation" in the output of
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md).

## Usage

``` r
cor_stats(df = NULL, predictors = NULL, quiet = FALSE, ...)
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

dataframe with columns `method` (with value "correlation"), `statistic`
and `value`

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
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

x <- cor_stats(
  df = vi_smol,
  predictors = vi_predictors_numeric
)

x
#>        method     statistic     value
#> 1 correlation             n 1081.0000
#> 2 correlation       minimum    0.0011
#> 3 correlation quantile_0.05    0.0426
#> 4 correlation quantile_0.25    0.1817
#> 5 correlation          mean    0.3980
#> 6 correlation        median    0.3610
#> 7 correlation quantile_0.75    0.6184
#> 8 correlation quantile_0.95    0.8264
#> 9 correlation       maximum    0.9893

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
