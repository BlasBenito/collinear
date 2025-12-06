# Compute variance inflation factors dataframe

Computes the pairwise correlation matrix between all pairs of predictors
via
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md)
and
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
applies
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md) to
the resulting matrix to compute Variance Inflation Factors, and returns
the result as a dataframe.

## Usage

``` r
vif_df(df = NULL, predictors = NULL, quiet = FALSE, ...)
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

dataframe with columns:

- `predictor`: Character, predictor name.

- `vif`: Numeric, variance inflation factor

## Variance Inflation Factors

VIF for predictor \\a\\ is computed as \\1/(1-R^2)\\, where \\R^2\\ is
the multiple R-squared from regressing \\a\\ on the other predictors.
Recommended maximums commonly used are 2.5, 5, and 10.

## References

- David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression
  Diagnostics: Identifying Influential Data and Sources of Collinearity.
  John Wiley & Sons. DOI: 10.1002/0471725153.

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md),
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Examples

``` r
data(vi_smol)

# ## OPTIONAL: parallelization setup
# ## irrelevant when all predictors are numeric
# ## only worth it for large data with many categoricals
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

# ## OPTIONAL: progress bar
# progressr::handlers(global = TRUE)

#predictors
predictors = c(
  "koppen_zone", #character
  "soil_type", #factor
  "topo_elevation", #numeric
  "soil_temperature_mean" #numeric
)

x <- vif_df(
  df = vi_smol,
  predictors = predictors
)
#> 
#> collinear::vif_df()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::vif_df()
#> └── collinear::cor_matrix()
#>     └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

x
#>       vif             predictor
#> 1  0.7927           koppen_zone
#> 2 -0.0356 soil_temperature_mean
#> 3 -0.4397             soil_type
#> 4 -0.9578        topo_elevation

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
