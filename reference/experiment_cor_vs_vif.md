# Dataframe with results of experiment comparing correlation and VIF thresholds

A dataframe summarizing 10,000 experiments comparing the output of
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
and
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md).
Each row records the input sampling parameters and the resulting
feature-selection metrics.

## Usage

``` r
data(experiment_cor_vs_vif)
```

## Format

A dataframe with 10,000 rows and 6 variables:

- input_rows:

  Number of rows in the input data subset.

- input_predictors:

  Number of predictors in the input data subset.

- output_predictors:

  Number of predictors selected by
  [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
  at the best-matching `max_vif`.

- max_cor:

  Maximum allowed pairwise correlation supplied to
  [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md).

- max_vif:

  VIF threshold at which
  [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
  produced the highest Jaccard similarity with
  [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
  for the given `max_cor`.

- out_selection_jaccard:

  Jaccard similarity between the predictors selected by
  [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
  and
  [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md).

## Details

The source data is a synthetic dataframe with 500 columns and 10,000
rows generated using `distantia::zoo_simulate()` with correlated time
series (`independent = FALSE`).

Each iteration randomly subsets 10-50 predictors and 30-100 rows per
predictor, applies
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
with a random `max_cor` threshold, then finds the `max_vif` value that
maximizes Jaccard similarity between the two selections.

## See also

Other experiments:
[`experiment_adaptive_thresholds`](https://blasbenito.github.io/collinear/reference/experiment_adaptive_thresholds.md),
[`gam_cor_to_vif`](https://blasbenito.github.io/collinear/reference/gam_cor_to_vif.md),
[`prediction_cor_to_vif`](https://blasbenito.github.io/collinear/reference/prediction_cor_to_vif.md)

## Examples

``` r
data(experiment_cor_vs_vif)
str(experiment_cor_vs_vif)
#> 'data.frame':    10000 obs. of  6 variables:
#>  $ input_rows           : num  1764 2250 1960 2392 1488 ...
#>  $ input_predictors     : num  42 45 40 26 48 47 49 40 43 40 ...
#>  $ output_predictors    : int  2 2 2 2 5 6 6 5 5 6 ...
#>  $ max_cor              : num  0.1 0.1 0.12 0.14 0.14 0.12 0.1 0.13 0.14 0.13 ...
#>  $ max_vif              : num  1 1 1 1 1.2 1.2 1.3 1.1 1.1 1.2 ...
#>  $ out_selection_jaccard: num  0.25 0.25 0.25 0.25 0.286 ...
```
