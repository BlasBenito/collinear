# Adaptive Filtering Thresholds

## Summary

The function
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
automatically configures multicollinearity filtering thresholds when the
arguments `max_cor` and `max_vif` are not specified.

This adaptive approach has several advantages:

- Eliminates the guesswork of choosing threshold values.
- Adapts filtering intensity to the correlation structure of the data.
- Keeps output VIF bounded within sensible limits (approximately 2.5 to
  7.5).
- Allows manual override when specific thresholds are needed.

This approach was validated across 10,000 simulated datasets with
varying correlation structures and predictor counts, consistently
producing output VIF values in the 2–7 range while retaining informative
predictors.

This article explains how the automatic threshold configuration works,
demonstrates its effectiveness across varied datasets, and provides
guidance on when manual configuration might be preferred.

## Setup

This article requires the following packages, setup, and example data.

``` r
library(collinear)
library(future)
library(ggplot2)
library(patchwork)
library(rstudioapi)

#parallelization setup
#only useful for categorical predictors
future::plan(
  future::multisession,
  workers = future::availableCores() - 1
)

#progress bar (does not work in Rmarkdown)
#progressr::handlers(global = TRUE)

#example data
data(
  #dataframe with predictors
  vi_smol, 
  #vector of predictor names
  vi_predictors_numeric,
  #dataframes with experiment data
  experiment_adaptive_thresholds,
  experiment_cor_vs_vif,
  #pre-trained GAM model
  gam_cor_to_vif
  )
```

## Adaptive Threshold Selection

The arguments `max_cor` and `max_vif` define the maximum pairwise
correlation and variance inflation factors allowed during
multicollinearity filtering.

Unlike
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
and
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md),
which use fixed defaults (`max_cor = 0.7`, `max_vif = 5`), the function
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
sets these to `NULL` and computes appropriate thresholds from the data.

Let’s see how that works.

The vector
[`vi_predictors_numeric`](https://blasbenito.github.io/collinear/articles/reference/vi_predictors_numeric.md)
names numeric predictors with a moderate multicollinearity, as the stats
below show.

``` r
collinear::collinear_stats(
  df = vi_smol,
  predictors = vi_predictors_numeric
) |> 
  dplyr::filter(
    statistic %in% c("quantile_0.75", "maximum")
  )
#>        method     statistic    value
#> 1 correlation quantile_0.75   0.6184
#> 2 correlation       maximum   0.9893
#> 3         vif quantile_0.75 354.2920
#> 4         vif       maximum 553.2944
```

Notice that the VIF scores are very large! To fix this issue can run
these predictors through
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).

``` r
x <- collinear::collinear(
  df = vi_smol,
  predictors = vi_predictors_numeric
)
#> 
#> collinear::collinear(): setting 'max_cor' to 0.618.
#> 
#> collinear::collinear(): setting 'max_vif' to 5.0318.
#> 
#> collinear::collinear()
#> └── collinear::validate_arg_preference_order()
#>     └── collinear::preference_order(): ranking 47 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::collinear(): selected predictors: 
#>  - topo_elevation
#>  - topo_slope
#>  - humidity_range
#>  - topo_diversity
#>  - soil_clay
#>  - cloud_cover_range
#>  - soil_silt
#>  - rainfall_min
#>  - growing_season_temperature
#>  - swi_max
#>  - soil_nitrogen
#>  - temperature_seasonality
#>  - rainfall_max
```

The first two messages above indicate the values of `max_cor` and
`max_vif` selected by
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
based on the data properties (more about that later).

The last message indicates the predictors selected in this run. Their
stats are shown below.

``` r
collinear::collinear_stats(
  df = x$result$df,
  predictors = x$result$selection
) |> 
  dplyr::filter(
    statistic %in% c("quantile_0.75", "maximum")
  )
#>        method     statistic  value
#> 1 correlation quantile_0.75 0.3342
#> 2 correlation       maximum 0.6109
#> 3         vif quantile_0.75 2.4148
#> 4         vif       maximum 3.4733
```

These stats show much more reasonable VIF scores now.

However, if you are aiming for specific multicollinearity thresholds,
the automatic setup can be overriden by providing the desired values for
`max_cor` and/or `max_vif`. If one of them is not defined, it will be
ignored.

``` r
x <- collinear::collinear(
  df = vi_smol,
  predictors = vi_predictors_numeric,
  max_cor = 0.5,
  max_vif = 2.5
)
#> 
#> collinear::collinear()
#> └── collinear::validate_arg_preference_order()
#>     └── collinear::preference_order(): ranking 47 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::collinear(): selected predictors: 
#>  - topo_elevation
#>  - topo_slope
#>  - humidity_range
#>  - soil_clay
#>  - cloud_cover_range
#>  - soil_silt
#>  - rainfall_min
#>  - growing_season_temperature
#>  - swi_max
#>  - soil_nitrogen

collinear::collinear_stats(
  df = x$result$df,
  predictors = x$result$selection
) |> 
  dplyr::filter(
    statistic %in% c("quantile_0.75", "maximum")
  )
#>        method     statistic  value
#> 1 correlation quantile_0.75 0.2926
#> 2 correlation       maximum 0.4573
#> 3         vif quantile_0.75 1.8224
#> 4         vif       maximum 2.1159
```

By adapting thresholds to each dataset’s structure,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
provides sensible defaults while still allowing manual override when
needed.

## Validation

To validate this adaptive threshold selection method I ran
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
on 10,000 random subsets of a synthetic dataset with 500 columns and
10,000 rows generated using `distantia::zoo_simulate()`. Each iteration
randomly selected 10-100 predictors and 30-100 rows per predictor, then
compared input and output multicollinearity statistics.

The experiment script can be opened in RStudio as follows:

``` r
system.file(
  "experiments/validation_adaptive_thresholds.R",
  package = "collinear"
  ) |> 
  rstudioapi::navigateToFile()
```

The results of this experiment are in the dataframe
[`experiment_adaptive_thresholds`](https://blasbenito.github.io/collinear/articles/reference/experiment_adaptive_thresholds.md),
plotted below.

![](reference/figures/README-unnamed-chunk-8-1.png)

The left panel shows the 75th percentile of input correlation against
the maximum VIF of the resulting selection. The cases below the VIF =
2.5 line (4% of all cases) are datasets with a small number of
predictors where multicollinearity drops sharply when one or several key
predictors are removed

The right panel shows the number of predictors before and after
multicollinearity filtering. The sublinear relationship indicates that
even with 100 input predictors, the selection stabilizes around 15-20
predictors.

These results indicate that the adaptive threshold selection works well
across most use cases.

## Step By Step

The adaptive selection of multicollinearity thresholds requires three
steps:

**1.** Compute the quantile 0.75 of the correlation for the input
predictors via
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md).
This quantile is used because it captures the upper tail of the
correlation distribution, where the problematic multicollinearity
begins.

``` r
cor_0.75 <- collinear::cor_stats(
  df = vi_smol,
  predictors = vi_predictors_numeric,
  quiet = TRUE
) |> 
  dplyr::filter(statistic == "quantile_0.75") |> 
  dplyr::pull(value)

cor_0.75
#> [1] 0.6184
```

**2.** Sigmoid transformation of the `cor_0.75` value. This sigmoid
function smoothly maps the input correlation to a bounded `max_cor`
threshold.

``` r
max_cor <- 0.493 + 0.242 / (1 + exp(-15 * (cor_0.75 - 0.614)))

max_cor
#> [1] 0.6179916
```

Where:

- `0.493`: floor of the curve, corresponding to VIF ≈ 2.5 in
  `prediction_cor_to_vif` (conservative filtering).
- `0.735`: ceiling of the curve (0.545 + 0.24), corresponding to VIF ≈
  7.5 in `prediction_cor_to_vif` (permissive filtering).
- `0.614`: midpoint where the transition is steepest.
- `-15`: steepness parameter controlling how sharply the curve
  transitions.

The full sigmoid curve is shown below.

![](reference/figures/README-unnamed-chunk-11-1.png)

Datasets with low correlation (quantile 0.75 \< 0.5) receive thresholds
near the floor, while highly correlated datasets (quantile 0.75 \> 0.8)
approach the ceiling. This prevents both over-filtering of clean
datasets and under-filtering of problematic ones.

3.  The pre-trained GAM model
    [`gam_cor_to_vif`](https://blasbenito.github.io/collinear/articles/reference/gam_cor_to_vif.md)
    predicts a suitable `max_vif` from the `max_cor` resulting from the
    transformation.

``` r
max_vif <- mgcv::predict.gam(
  object = collinear::gam_cor_to_vif,
  newdata = data.frame(max_cor = max_cor)
) 

max_vif
#>        1 
#> 5.031596
```

This model was fitted on the simulation results in
[`experiment_cor_vs_vif`](https://blasbenito.github.io/collinear/articles/reference/experiment_cor_vs_vif.md),
where both filtering methods were applied across 10,000 random dataset
configurations. For each `max_cor` value, the `max_vif` producing the
highest Jaccard similarity between the two selections was identified.

The experiment script can be opened in RStudio as follows:

``` r
system.file(
  "experiments/relationship_cor_vs_vif.R",
  package = "collinear"
) |> 
  rstudioapi::navigateToFile()
```

The model uses squared Jaccard similarity as weights to emphasize cases
where
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
and
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
achieved strong agreement.

``` r
m <- mgcv::gam(
  formula = max_vif ~ s(max_cor, k = 6),
  weights = experiment_cor_vs_vif$out_selection_jaccard^2,
  data = experiment_cor_vs_vif
)
```

The plot below shows the simulation results and the fitted model.

![](reference/figures/README-unnamed-chunk-15-1.png)

The curve tracks through the high-similarity region (red/orange points),
indicating that the model successfully captures the relationship between
`max_cor` and `max_vif` for cases where both methods agree.

## When to Override

While the adaptive defaults work well for most cases, consider setting
thresholds manually when:

- **Strict coefficient interpretability is required**: Set
  `max_vif = 2.5` or lower for models where coefficient stability is
  critical.

- **Maximizing predictor retention**: Set `max_cor = 0.9` and
  `max_vif = 10` for prediction-focused models where some
  multicollinearity is acceptable.

- **Domain-specific requirements**: Some fields have established VIF
  thresholds (e.g., VIF \< 5 or VIF \< 10) that should be used for
  consistency with existing literature.
