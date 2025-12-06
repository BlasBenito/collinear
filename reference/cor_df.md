# Compute signed pairwise correlations dataframe

Computes pairwise correlations between predictors using appropriate
methods for different variable types:

- **Numeric vs. Numeric**: Pearson correlation via
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html).

- **Numeric vs. Categorical**: Target-encodes the categorical variable
  using the numeric variable as reference via
  [`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)
  with leave-one-out method, then computes Pearson correlation.

- **Categorical vs. Categorical**: Cramer's V via
  [`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md)
  as a measure of association. See
  [`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md)
  for important notes on mixing Pearson correlation and Cramer's V in
  multicollinearity analysis.

Parallelization via
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
and progress bars via
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
are supported but only beneficial for large datasets with categorical
predictors. Numeric-only correlations do not use parallelization or
progress bars. Example: With 16 workers, 30k rows (dataframe
[vi](https://blasbenito.github.io/collinear/reference/vi.md)), 49
numeric and 12 categorical predictors (see
[vi_predictors](https://blasbenito.github.io/collinear/reference/vi_predictors.md)),
parallelization achieves a 5.4x speedup (147s → 27s).

## Usage

``` r
cor_df(df = NULL, predictors = NULL, quiet = FALSE, ...)
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

- `x`: character, first predictor name.

- `y`: character, second predictor name.

- `correlation`: numeric, Pearson correlation (numeric vs. numeric and
  numeric vs. categorical) or Cramer's V (categorical vs. categorical).

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md),
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md),
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Examples

``` r
data(vi_smol)

## OPTIONAL: parallelization setup
## irrelevant when all predictors are numeric
## only worth it for large data with many categoricals
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

## OPTIONAL: progress bar
# progressr::handlers(global = TRUE)

#predictors
predictors = c(
  "koppen_zone", #character
  "soil_type", #factor
  "topo_elevation", #numeric
  "soil_temperature_mean" #numeric
)

x <- cor_df(
  df = vi_smol,
  predictors = predictors
)
#> 
#> collinear::cor_df()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

x
#>                       x                     y correlation     metric
#> 1 soil_temperature_mean           koppen_zone   0.9195774    Pearson
#> 2 soil_temperature_mean             soil_type   0.6306982    Pearson
#> 3        topo_elevation           koppen_zone   0.5413656    Pearson
#> 4        topo_elevation             soil_type   0.3458931    Pearson
#> 5           koppen_zone             soil_type   0.3146128 Cramer's V
#> 6        topo_elevation soil_temperature_mean  -0.2837184    Pearson

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
