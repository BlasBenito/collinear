# VIF Statistics

Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25,
median (0.5), 0.75, and 0.95 of the column "vif" in the output of
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md).

## Usage

``` r
vif_stats(df = NULL, predictors = NULL, quiet = FALSE, ...)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
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

dataframe with columns `method` with the value "vif", `statistic` with
the statistic name, and `value`.

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md),
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md)

## Examples

``` r
data(
  vi_smol,
  vi_predictors_numeric
  )

# ## OPTIONAL: parallelization setup
# ## irrelevant when all predictors are numeric
# ## only worth it for large data with many categoricals
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

# ## OPTIONAL: progress bar
# progressr::handlers(global = TRUE)

x <- vif_stats(
  df = vi_smol,
  predictors = vi_predictors_numeric
)

x
#>   method     statistic    value
#> 1    vif             n  47.0000
#> 2    vif       minimum   1.8158
#> 3    vif quantile_0.05   3.7499
#> 4    vif quantile_0.25  58.1814
#> 5    vif          mean 214.5167
#> 6    vif        median 170.1444
#> 7    vif quantile_0.75 354.2920
#> 8    vif quantile_0.95 520.9411
#> 9    vif       maximum 553.2944

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
