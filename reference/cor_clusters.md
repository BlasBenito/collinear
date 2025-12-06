# Group predictors by hierarchical correlation clustering

Hierarchical clustering of predictors from their correlation matrix.
Computes the correlation matrix with
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md)
and
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
transforms it to a distance matrix using
[`stats::dist()`](https://rdrr.io/r/stats/dist.html), computes a
clustering solution with
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html), and applies
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html) to separate
groups based on the value of the argument `max_cor`.

Returns a dataframe with predictor names and their clusters, and
optionally, prints a dendrogram of the clustering solution.

Accepts a parallelization setup via
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
and a progress bar via
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
(see examples).

## Usage

``` r
cor_clusters(
  df = NULL,
  predictors = NULL,
  max_cor = 0.7,
  method = "complete",
  quiet = FALSE,
  ...
)
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

- max_cor:

  (optional; numeric or NULL) Correlation value used to separate
  clustering groups. Valid values are between 0.01 and 0.99. Default:
  0.7

- method:

  (optional, character string) Argument of
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) defining the
  agglomerative method. One of: "ward.D", "ward.D2", "single",
  "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (=
  WPGMC) or "centroid" (= UPGMC). Unambiguous abbreviations are accepted
  as well. Default: "complete".

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

list:

- df: dataframe with predictor names and their cluster IDs.

- hclust: clustering object

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
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

#group predictors using max_cor as clustering threshold
clusters <- cor_clusters(
  df = vi_smol,
  predictors = c(
    "koppen_zone", #character
    "soil_type", #factor
    "topo_elevation", #numeric
    "soil_temperature_mean" #numeric
  ),
  max_cor = 0.75
)
#> 
#> collinear::cor_clusters()
#> └── collinear::cor_matrix()
#>     └── collinear::cor_df()
#>         └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::cor_clusters()
#> └── collinear::cor_matrix()
#>     └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

#clusters dataframe
clusters$df
#>               predictor cluster
#> 1           koppen_zone       1
#> 2 soil_temperature_mean       1
#> 3             soil_type       2
#> 4        topo_elevation       3

##plot hclust object
# graphics::plot(clusters$hclust)

##plot max_cor threshold
# graphics::abline(
#   h = 1 - 0.75,
#   col = "red4",
#   lty = 3,
#   lwd = 2
# )

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
