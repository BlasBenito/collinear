
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `collinear` Automated Multicollinearity Management <a href="https://github.com/BlasBenito/collinear"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- Development badges 
&#10;[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)
[![Devel-version](https://img.shields.io/badge/devel%20version-1.0.1-blue.svg)](https://github.com/blasbenito/collinear)
&#10;<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10039396.svg)](https://doi.org/10.5281/zenodo.10039396)
[![CRAN
status](https://www.r-pkg.org/badges/version/collinear)](https://cran.r-project.org/package=collinear)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/collinear)](https://CRAN.R-project.org/package=collinear)
[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Summary

The R package `collinear` provides a comprehensive toolkit for smart
multicollinearity management in datasets with mixed variable types. The
main function,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.html),
integrates five core components:

- [**Target
  Encoding**](https://blasbenito.github.io/collinear/articles/target_encoding.html)
  (function
  [target_encoding_lab()](https://blasbenito.github.io/collinear/reference/target_encoding_lab.html)):
  Transparently converts categorical predictors to numeric when
  required, enabling VIF and correlation analysis across mixed data
  types.

- [**Intelligent Predictor
  Ranking**](https://blasbenito.github.io/collinear/articles/intelligent_predictor_ranking.html)
  (function
  [preference_order()](https://blasbenito.github.io/collinear/reference/preference_order.html)):
  Prioritizes predictors by their univariate association with the
  response to ensure that the most relevant ones are retained during
  filtering.

- [**Unified Correlation
  Framework**](https://blasbenito.github.io/collinear/articles/unified_correlation_framework.html)
  (function
  [cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.html)):
  Computes pairwise correlations between any variable types using
  Pearson correlation (numeric-numeric), target encoding
  (numeric-categorical), and Cramer’s V (categorical-categorical) within
  a single, consistent workflow.

- [**Adaptive Filtering
  Thresholds**](https://blasbenito.github.io/collinear/articles/adaptive_filtering_thresholds.html)
  (function
  [collinear()](https://blasbenito.github.io/collinear/reference/collinear.html)):
  Automatically configures correlation and VIF thresholds based on each
  dataset’s correlation structure, eliminating guesswork while allowing
  manual override.

- [**Dual Filtering
  Strategy**](https://blasbenito.github.io/collinear/articles/dual_filtering_strategy.html)
  (function
  [collinear_select()](https://blasbenito.github.io/collinear/reference/collinear_select.html)):
  Combines pairwise correlation and Variance Inflation Factor filtering
  while considering predictor rankings to manage multicollinearity while
  maximizing the predictive power of the resulting selection of
  predictors.

These methods, except target encoding are also fully integrated into the
`tidymodels` implementation
[step_collinear()](https://blasbenito.github.io/collinear/reference/step_collinear.html).

The package also provides diagnostic functions
([cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.html),
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.html),
[collinear_stats()](https://blasbenito.github.io/collinear/reference/collinear_stats.html))
to help users assess multicollinearity in their data before and after
filtering.

## Install

The package `collinear` can be installed from CRAN.

``` r
install.packages("collinear")
```

The development version can be installed from GitHub.

``` r
remotes::install_github(
  repo = "blasbenito/collinear", 
  ref = "development"
  )
```

Previous versions are in the `archive_xxx` branches of the GitHub
repository.

``` r
remotes::install_github(
  repo = "blasbenito/collinear", 
  ref = "archive_v2.0.0"
  )
```

## Getting Started

### Setup

Most functions in the `collinear` package support parallelization and
progress bars via [`future`](https://future.futureverse.org/) and
[`progressr`](https://progressr.futureverse.org/). Parallelization,
however, is only advantageous for large datasets including categorical
predictors.

``` r
library(collinear)
library(future)
library(progressr)

future::plan(
  future::multisession,
  workers = future::availableCores() - 1
  )

#does not work in Rmarkdown
#progressr::handlers(global = TRUE)
```

### Example Data

The example dataframe
[vi_smol](https://blasbenito.github.io/collinear/reference/vi_smol.html)
has several response variables and a large set of predictors. Here we
focus on the numeric response `vi_numeric`, and the complete set of
numeric and categorical predictors, stored in the vector
[vi_predictors](https://blasbenito.github.io/collinear/reference/vi_predictors.html).

``` r
data(vi_smol, vi_predictors, package = "spatialData")
nrow(vi_smol)
#> [1] 580
length(vi_predictors)
#> [1] 58
```

### Multicollinearity Analysis

The package provides several functions to assess multicollinearity.

The functions
[cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.html)
and
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.html)
generate dataframes with the pairwise correlations and VIF scores of the
predictors, while the functions
[cor_stats()](https://blasbenito.github.io/collinear/reference/cor_stats.html),
[vif_stats()](https://blasbenito.github.io/collinear/reference/vif_stats.html)
(used below) and
[collinear_stats()](https://blasbenito.github.io/collinear/reference/collinear_stats.html)
generate descriptive multicollinearity statistics. Notice that the
function takes a while to execute because computing correlations for the
categorical variables in `vi_predictors` is computationally expensive.

``` r
collinear::vif_stats(
  df = vi_smol,
  predictors = vi_predictors,
  quiet = TRUE
)
#> Warning: 
#> collinear::vif_stats()
#> └── collinear::vif_df()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 11 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#>   method     statistic    value
#> 1    vif             n  58.0000
#> 2    vif       minimum  -6.0304
#> 3    vif quantile_0.05  -1.0855
#> 4    vif quantile_0.25   4.5649
#> 5    vif          mean 105.5328
#> 6    vif        median  50.9527
#> 7    vif quantile_0.75 124.4136
#> 8    vif quantile_0.95 409.8273
#> 9    vif       maximum 835.4799
```

The quantile 0.75 returned by `vif_stats()` indicates that 25% of the
predictors have a VIF score higher than 6.5. This suggests substantial
redundancy in the set of predictors.

### Multicollinearity Filtering

To reduce multicollinearity in `vi_smol` we apply
[collinear()](https://blasbenito.github.io/collinear/reference/collinear.html)
with a minimal setup.

``` r
x <- collinear::collinear(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors
)
#> 
#> collinear::collinear()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#>  - koppen_group
#>  - koppen_description
#>  - biogeo_ecoregion
#>  - biogeo_biome
#>  - biogeo_realm
#>  - country_name
#>  - continent
#>  - region
#>  - subregion
#> 
#> collinear::collinear()
#> └── collinear::cor_df(): 11 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::collinear(): setting 'max_cor' to 0.6846.
#> 
#> collinear::collinear(): setting 'max_vif' to 6.5354.
#> 
#> collinear::collinear()
#> └── collinear::preference_order()
#>     └── collinear::f_auto(): selected function 'f_numeric_rf()' to compute preference order.
#> 
#> collinear::collinear()
#> └── collinear::collinear_select()
#>     └── collinear::vif(): some VIF values exceeded 1M and were set to Inf.
#> 
#> collinear::collinear(): selected predictors: 
#>  - rainfall_mean
#>  - swi_mean
#>  - evapotranspiration_max
#>  - evapotranspiration_range
#>  - swi_min
#>  - soil_soc
#>  - humidity_range
#>  - topo_elevation
#>  - cloud_cover_range
#>  - continent
#>  - soil_sand
#>  - soil_clay
#>  - topo_diversity
#>  - topo_slope
```

The function returns an object of class `collinear_output`, that has its
own `print()` and `summary()` methods.

``` r
x
#> Result
#> ===================
#>  - response: vi_numeric
#>    --------------------
#> 
#>  + df:
#>    - rows: 580
#>    - cols: 15
#> 
#>  + preference order:
#>    + df:
#>      - rows: 58
#>      - cols: 6
#>    + preference:
#>      - rainfall_mean
#>      - growing_season_rainfall
#>      - growing_season_length
#>      - swi_mean
#>      - aridity_index
#>    + f: f_numeric_rf
#> 
#>  + selection:
#>    - rainfall_mean
#>    - swi_mean
#>    - evapotranspiration_max
#>    - evapotranspiration_range
#>    - swi_min
#>    - ... (9 ommited)
#> 
#>  + formulas:
#>    - linear: vi_numeric ~ rainfall_mean + swi_mean + evapotranspiration_max + evapotranspiration_range + swi_min + ... (9 terms omitted) 
#>    - smooth: vi_numeric ~ s(rainfall_mean) + s(swi_mean) + s(evapotranspiration_max) + s(evapotranspiration_range) + s(swi_min) + ... (9 terms omitted)
```

The object `df` contains the response and the selected predictors.

``` r
colnames(x$vi_numeric$df)
#>  [1] "vi_numeric"               "rainfall_mean"           
#>  [3] "swi_mean"                 "evapotranspiration_max"  
#>  [5] "evapotranspiration_range" "swi_min"                 
#>  [7] "soil_soc"                 "humidity_range"          
#>  [9] "topo_elevation"           "cloud_cover_range"       
#> [11] "continent"                "soil_sand"               
#> [13] "soil_clay"                "topo_diversity"          
#> [15] "topo_slope"
```

The object `preference_order` contains the ranking of predictors. It is
computed by the function
[preference_order()](https://blasbenito.github.io/collinear/reference/preference_order.html)
by assessing the association between the response and the predictors. In
this case, it fits univariate models between the response and each
predictor using
[f_numeric_rf](https://blasbenito.github.io/collinear/reference/f_numeric_rf.html),
and returns the R-squared of the observations vs the model predictions.
This ranking ensures that the most important predictors are protected
during multicollinearity filtering.

``` r
head(x$vi_numeric$preference_order)
#>     response               predictor            f    metric  score rank
#> 1 vi_numeric           rainfall_mean f_numeric_rf R-squared 0.8830    1
#> 2 vi_numeric growing_season_rainfall f_numeric_rf R-squared 0.8716    2
#> 3 vi_numeric   growing_season_length f_numeric_rf R-squared 0.8660    3
#> 4 vi_numeric                swi_mean f_numeric_rf R-squared 0.8330    4
#> 5 vi_numeric           aridity_index f_numeric_rf R-squared 0.8276    5
#> 6 vi_numeric             koppen_zone f_numeric_rf R-squared 0.8160    6
```

The object `selection` contains non-collinear predictors chosen by
taking into account their pairwise correlation and VIF against all other
predictors, and their position in the ranking above.

``` r
x$vi_numeric$selection
#>  [1] "rainfall_mean"            "swi_mean"                
#>  [3] "evapotranspiration_max"   "evapotranspiration_range"
#>  [5] "swi_min"                  "soil_soc"                
#>  [7] "humidity_range"           "topo_elevation"          
#>  [9] "cloud_cover_range"        "continent"               
#> [11] "soil_sand"                "soil_clay"               
#> [13] "topo_diversity"           "topo_slope"              
#> attr(,"validated")
#> [1] TRUE
```

We can check that this selection of predictors shows low
multicollinearity by running the function
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.html)
on them.

``` r
collinear::vif_df(
  df = x$vi_numeric$df,
  predictors = x$vi_numeric$selection
)
#>        vif                predictor
#> 1   4.9323        cloud_cover_range
#> 2   3.4887                continent
#> 3   3.3868   evapotranspiration_max
#> 4   3.3125 evapotranspiration_range
#> 5   3.1740           humidity_range
#> 6   2.1249            rainfall_mean
#> 7   2.0856                soil_clay
#> 8   2.0259                soil_sand
#> 9   1.8021                 soil_soc
#> 10  1.6395                 swi_mean
#> 11  1.5953                  swi_min
#> 12  1.5152           topo_diversity
#> 13  1.4237           topo_elevation
#> 14 -0.1557               topo_slope
```

All VIF scores are below 2.5, `collinear()` did a good job here!

Finally, `collinear()` also returns modeling formulas to help kick start
exploratory modelling.

``` r
x$vi_numeric$formulas
#> $linear
#> vi_numeric ~ rainfall_mean + swi_mean + evapotranspiration_max + 
#>     evapotranspiration_range + swi_min + soil_soc + humidity_range + 
#>     topo_elevation + cloud_cover_range + continent + soil_sand + 
#>     soil_clay + topo_diversity + topo_slope
#> <environment: 0x5ca0746a67c0>
#> 
#> $smooth
#> vi_numeric ~ s(rainfall_mean) + s(swi_mean) + s(evapotranspiration_max) + 
#>     s(evapotranspiration_range) + s(swi_min) + s(soil_soc) + 
#>     s(humidity_range) + s(topo_elevation) + s(cloud_cover_range) + 
#>     continent + s(soil_sand) + s(soil_clay) + s(topo_diversity) + 
#>     s(topo_slope)
#> <environment: 0x5ca0746a67c0>
```

The function returns linear formulas for numeric outcomes, and
classification formulas for categorical outcomes.

### Model Fitting

The output of `collinear()` can be used right away to fit exploratory
models, as shown below.

``` r
m <- lm(
  formula = x$vi_numeric$formulas$linear, 
  data = x$vi_numeric$df,
  na.action = na.omit
  )

summary(m)
#> 
#> Call:
#> lm(formula = x$vi_numeric$formulas$linear, data = x$vi_numeric$df, 
#>     na.action = na.omit)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.42086 -0.04433  0.00069  0.04656  0.23475 
#> 
#> Coefficients:
#>                            Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)               2.444e-01  6.263e-02   3.902 0.000107 ***
#> rainfall_mean             3.900e-05  7.425e-06   5.252 2.14e-07 ***
#> swi_mean                  6.849e-03  4.593e-04  14.910  < 2e-16 ***
#> evapotranspiration_max   -1.165e-03  1.759e-04  -6.624 8.24e-11 ***
#> evapotranspiration_range -1.928e-04  1.449e-04  -1.330 0.183968    
#> swi_min                  -2.014e-03  6.539e-04  -3.080 0.002168 ** 
#> soil_soc                 -4.742e-04  1.989e-04  -2.384 0.017453 *  
#> humidity_range           -1.145e-03  6.473e-04  -1.769 0.077474 .  
#> topo_elevation           -4.054e-05  7.512e-06  -5.397 1.00e-07 ***
#> cloud_cover_range         6.015e-04  3.665e-04   1.641 0.101330    
#> continentAsia            -2.660e-02  1.280e-02  -2.078 0.038199 *  
#> continentEurope           4.071e-02  1.995e-02   2.041 0.041765 *  
#> continentNorth America    6.344e-02  1.595e-02   3.977 7.90e-05 ***
#> continentOceania          3.910e-02  1.579e-02   2.476 0.013573 *  
#> continentSouth America    5.631e-02  1.296e-02   4.345 1.65e-05 ***
#> soil_sand                 9.846e-04  4.855e-04   2.028 0.043043 *  
#> soil_clay                 1.127e-03  8.111e-04   1.390 0.165089    
#> topo_diversity            3.321e-03  9.168e-04   3.622 0.000319 ***
#> topo_slope                4.702e-03  1.457e-03   3.227 0.001324 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.08245 on 558 degrees of freedom
#>   (3 observations deleted due to missingness)
#> Multiple R-squared:  0.852,  Adjusted R-squared:  0.8472 
#> F-statistic: 178.4 on 18 and 558 DF,  p-value: < 2.2e-16
```

### Integration with `tidymodels`

The function
[step_collinear()](https://blasbenito.github.io/collinear/reference/step_collinear.html)
wraps
[collinear()](https://blasbenito.github.io/collinear/reference/collinear.html)
to facilitate its usage in `tidymodels` recipes. Please notice that
`step_collinear()` does not perform target encoding, as combining this
functionality with multicollinearity filtering does not fit well with
how `recipes` works.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(recipes)
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
library(parsnip)
library(workflows)

# model formula
vi_formula <- collinear::model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors
)

# recipe
vi_recipe <- recipes::recipe(
  formula = vi_formula,
  data = vi_smol
  ) |>
  #multicollinearity filtering
  collinear::step_collinear(
    recipes::all_predictors()
  )

#linear model
vi_model <- parsnip::rand_forest() |>
  parsnip::set_engine("ranger", importance = "permutation") |>
  parsnip::set_mode("regression")

#create and fit workflow
vi_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

# create and fit workflow
vi_workflow <- workflows::workflow() |>
  workflows::add_recipe(vi_recipe) |>
  workflows::add_model(vi_model) |>
  workflows::fit(data = vi_smol)
#> Warning: 
#> collinear::collinear()
#> └── collinear::cor_df(): 11 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

vi_workflow
#> ══ Workflow [trained] ══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 1 Recipe Step
#> 
#> • step_collinear()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>              (Intercept)             rainfall_mean                  swi_mean  
#>                2.444e-01                 3.900e-05                 6.849e-03  
#>   evapotranspiration_max  evapotranspiration_range                   swi_min  
#>               -1.165e-03                -1.928e-04                -2.014e-03  
#>                 soil_soc            humidity_range            topo_elevation  
#>               -4.742e-04                -1.145e-03                -4.054e-05  
#>        cloud_cover_range             continentAsia           continentEurope  
#>                6.015e-04                -2.660e-02                 4.071e-02  
#>   continentNorth America          continentOceania    continentSouth America  
#>                6.344e-02                 3.910e-02                 5.631e-02  
#>                soil_sand                 soil_clay            topo_diversity  
#>                9.846e-04                 1.127e-03                 3.321e-03  
#>               topo_slope  
#>                4.702e-03
```

## Citation

If you find this package useful, please cite it as:

*Blas M. Benito (2025). collinear: R Package for Automated
Multicollinearity Management. Version 3.0.0. doi:
10.5281/zenodo.10039396*

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
