# Dataframe resulting from experiment to test the automatic selection of multicollinearity thresholds

A dataframe summarizing 10,000 experiments validating the adaptive
multicollinearity threshold system in
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).
Each row records input data characteristics and the resulting
multicollinearity metrics after filtering.

## Usage

``` r
data(experiment_adaptive_thresholds)
```

## Format

A dataframe with 10,000 rows and 9 variables:

- input_rows:

  Number of rows in the input data subset.

- input_predictors:

  Number of predictors in the input data subset.

- output_predictors:

  Number of predictors retained after filtering.

- input_cor_q75:

  75th percentile of pairwise correlations in the input data.

- output_cor_q75:

  75th percentile of pairwise correlations in the selected predictors.

- input_cor_max:

  Maximum pairwise correlation in the input data.

- output_cor_max:

  Maximum pairwise correlation in the selected predictors.

- input_vif_max:

  Maximum VIF in the input data.

- output_vif_max:

  Maximum VIF in the selected predictors.

## Details

The source data is a synthetic dataframe with 500 columns and 10,000
rows generated using `distantia::zoo_simulate()` with correlated time
series (`independent = FALSE`, `seasons = 0`).

Each iteration randomly subsets 10-100 predictors and 30-100 rows per
predictor, then applies
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
with automatic threshold configuration to assess:

- Whether output VIF stays bounded between ~2.5 and ~7.5

- How the system adapts to different correlation structures

- How predictor retention scales with input size

## See also

Other experiments:
[`experiment_cor_vs_vif`](https://blasbenito.github.io/collinear/reference/experiment_cor_vs_vif.md),
[`gam_cor_to_vif`](https://blasbenito.github.io/collinear/reference/gam_cor_to_vif.md),
[`prediction_cor_to_vif`](https://blasbenito.github.io/collinear/reference/prediction_cor_to_vif.md)

## Examples

``` r
data(experiment_adaptive_thresholds)
str(experiment_adaptive_thresholds)
#> 'data.frame':    10000 obs. of  9 variables:
#>  $ input_rows       : int  806 495 968 1100 1530 1014 660 1248 627 1020 ...
#>  $ input_predictors : int  13 11 11 11 17 13 11 13 11 15 ...
#>  $ output_predictors: int  5 5 6 4 5 6 6 5 5 5 ...
#>  $ input_cor_q75    : num  0.476 0.389 0.356 0.583 0.663 ...
#>  $ output_cor_q75   : num  0.25 0.162 0.251 0.303 0.235 ...
#>  $ input_cor_max    : num  0.927 0.819 0.85 0.815 0.942 ...
#>  $ output_cor_max   : num  0.336 0.379 0.36 0.458 0.423 ...
#>  $ input_vif_max    : num  27.69 10.37 9.71 8.7 30.47 ...
#>  $ output_vif_max   : num  1.25 1.26 1.31 1.32 1.32 ...
```
