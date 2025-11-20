
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `collinear` Automated Multicollinearity Management <a href="https://github.com/BlasBenito/collinear"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- Development badges 
&#10;[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)
[![Devel-version](https://img.shields.io/badge/devel%20version-1.0.1-blue.svg)](https://github.com/blasbenito/collinear)
&#10;<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10039489.svg)](https://doi.org/10.5281/zenodo.10039489)
[![CRAN
status](https://www.r-pkg.org/badges/version/collinear)](https://cran.r-project.org/package=collinear)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/collinear)](https://CRAN.R-project.org/package=collinear)
[![R-CMD-check](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/collinear/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Summary

The R package `collinear` provides a comprehensive toolkit for smart
multicollinearity management in datasets with mixed variable types. The
main function, \[collinear()\], integrates five core components to
handle multicollinearity across numeric and categorical predictors while
protecting important predictors:

- **Target Encoding** (function \[target_encoding_lab()\]):
  Transparently converts categorical predictors to numeric when
  required, enabling VIF and correlation analysis across mixed data
  types.

- **Intelligent Predictor Ranking** (function \[preference_order()\]):
  Prioritizes predictors by their association with the response, or
  allows a user-defined ranking to ensure that the most relevant ones
  are retained during filtering.

- **Unified Correlation Framework** (function \[cor_df()\]): Computes
  pairwise correlations between any variable types using Pearson
  correlation (numeric-numeric), target encoding (numeric-categorical),
  and Cramer’s V (categorical-categorical) within a single, consistent
  workflow.

- **Adaptive Filtering Thresholds** (function \[collinear()\]):
  Automatically configures correlation and VIF thresholds based on each
  dataset’s correlation structure, eliminating guesswork while allowing
  manual override.

- **Dual Filtering Strategy** (function \[collinear_select()\]):
  Combines pairwise correlation and Variance Inflation Factor filtering
  while considering predictor rankings to manage multicollinearity while
  maximizing the predictive power of the resulting selection of
  predictors.

These methods are fully integrated into the function \[collinear()\] and
its `tidymodels` wrapper, \[step_collinear()\]. This function handles
the complete multicollinearity filtering workflow, from data validation
to variable selection, returning filtered datasets and modeling
formulas.

The package also provides diagnostic functions (\[cor_df()\],
\[vif_df()\], \[collinear_stats()\]) to help users assess
multicollinearity in their data before and after filtering.

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

The example dataframe \[vi_smol\] has several response variables and a
large set of predictors. Here we focus on the numeric response
`vi_numeric`, and the complete set of numeric and categorical
predictors, stored in the vector \[vi_predictors\].

``` r
data(vi_smol, vi_predictors)
nrow(vi_smol)
#> [1] 610
length(vi_predictors)
#> [1] 58
```

### Multicollinearity Analysis

The package provides several functions to assess multicollinearity.

The functions \[cor_df()\] and \[vif_df()\] generate dataframes with the
pairwise correlations and VIF scores of the predictors, while the
functions \[cor_stats()\], \[vif_stats()\] (used below) and
\[collinear_stats()\] generate descriptive multicollinearity statistics.

``` r
collinear::vif_stats(
  df = vi_smol,
  predictors = vi_predictors,
  quiet = TRUE
)
#>   method     statistic   value
#> 1    vif             n 58.0000
#> 2    vif       minimum  0.1788
#> 3    vif quantile_0.05  0.3865
#> 4    vif quantile_0.25  1.2504
#> 5    vif          mean  4.9427
#> 6    vif        median  2.7136
#> 7    vif quantile_0.75  6.5669
#> 8    vif quantile_0.95 13.0459
#> 9    vif       maximum 41.9967
```

The quantile 0.75 returned by `vif_stats()` indicates that 25% of the
predictors have a VIF score higher than 6. This suggests substantial
redundancy in the set of predictors.

### Multicollinearity Filtering

The function \[collinear()\] is designed to reduce multicollinearity in
complex modelling data with a minimal setup.

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
#> collinear::collinear(): setting 'max_cor' to 0.58.
#> 
#> collinear::collinear(): setting 'max_vif' to 2.5.
#> 
#> collinear::collinear()
#> └── collinear::preference_order()
#>     └── collinear::f_auto(): selected function 'f_numeric_rf()' to compute preference order.
#> 
#> collinear::collinear(): selected predictors: 
#>  - rainfall_mean
#>  - soil_temperature_max
#>  - swi_range
#>  - temperature_seasonality
#>  - humidity_range
#>  - topo_elevation
#>  - soil_sand
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
#>    - rows: 610
#>    - cols: 10
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
#>    - soil_temperature_max
#>    - swi_range
#>    - temperature_seasonality
#>    - humidity_range
#>    - ... (4 ommited)
#> 
#>  + formulas:
#>    - linear: vi_numeric ~ rainfall_mean + soil_temperature_max + swi_range + temperature_seasonality + humidity_range + ... (4 terms omitted) 
#>    - smooth: vi_numeric ~ s(rainfall_mean) + s(soil_temperature_max) + s(swi_range) + s(temperature_seasonality) + s(humidity_range) + ... (4 terms omitted)
```

The object `df` contains the response and the selected predictors.

``` r
colnames(x$vi_numeric$df)
#>  [1] "vi_numeric"              "rainfall_mean"          
#>  [3] "soil_temperature_max"    "swi_range"              
#>  [5] "temperature_seasonality" "humidity_range"         
#>  [7] "topo_elevation"          "soil_sand"              
#>  [9] "topo_diversity"          "topo_slope"
```

The object `preference_order` contains the ranking of predictors. It is
computed by the function \[preference_order()\] by assessing the
association between the response and the predictors. In this case, it
fits univariate models between the response and each predictor using
\[f_numeric_rf\], and returns the R-squared of the observations vs the
model predictions. This ranking ensures that the most important
predictors are protected during multicollinearity filtering.

``` r
head(x$vi_numeric$preference_order)
#>     response               predictor            f    metric  score rank
#> 1 vi_numeric           rainfall_mean f_numeric_rf R-squared 0.8835    1
#> 2 vi_numeric growing_season_rainfall f_numeric_rf R-squared 0.8731    2
#> 3 vi_numeric   growing_season_length f_numeric_rf R-squared 0.8644    3
#> 4 vi_numeric                swi_mean f_numeric_rf R-squared 0.8333    4
#> 5 vi_numeric           aridity_index f_numeric_rf R-squared 0.8279    5
#> 6 vi_numeric             koppen_zone f_numeric_rf R-squared 0.8174    6
```

The object `selection` contains non-collinear predictors chosen by
taking into account their pairwise correlation and VIF against all other
predictors, and their position in the ranking above.

``` r
x$vi_numeric$selection
#> [1] "rainfall_mean"           "soil_temperature_max"   
#> [3] "swi_range"               "temperature_seasonality"
#> [5] "humidity_range"          "topo_elevation"         
#> [7] "soil_sand"               "topo_diversity"         
#> [9] "topo_slope"
```

We can check that this selection of predictors shows low
multicollinearity by running the function \[vif_df()\] on them.

``` r
collinear::vif_df(
  df = x$vi_numeric$df,
  predictors = x$vi_numeric$selection
)
#>      vif               predictor
#> 1 2.4773          humidity_range
#> 2 2.1802           rainfall_mean
#> 3 2.0626               soil_sand
#> 4 1.7707    soil_temperature_max
#> 5 1.7192               swi_range
#> 6 1.5906 temperature_seasonality
#> 7 1.3148          topo_diversity
#> 8 1.1651          topo_elevation
#> 9 1.1454              topo_slope
```

All VIF scores are below 2.5, `collinear()` did a good job here!

Finally, `collinear()` also returns modeling formulas to help kick start
exploratory modelling.

``` r
x$vi_numeric$formulas
#> $linear
#> vi_numeric ~ rainfall_mean + soil_temperature_max + swi_range + 
#>     temperature_seasonality + humidity_range + topo_elevation + 
#>     soil_sand + topo_diversity + topo_slope
#> <environment: 0x5d5d662f75c0>
#> 
#> $smooth
#> vi_numeric ~ s(rainfall_mean) + s(soil_temperature_max) + s(swi_range) + 
#>     s(temperature_seasonality) + s(humidity_range) + s(topo_elevation) + 
#>     s(soil_sand) + s(topo_diversity) + s(topo_slope)
#> <environment: 0x5d5d662f75c0>
```

The function returns linear formulas for numeric outcomes, and
classification formulas for categorical outcomes.

### Model Fitting

The output of `collinear()` can be used right away to fit exploratory
models, as shown below.

``` r
m <- mgcv::gam(
  formula = x$vi_numeric$formulas$smooth, 
  data = x$vi_numeric$df,
  na.action = na.omit
  )

summary(m)
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> vi_numeric ~ s(rainfall_mean) + s(soil_temperature_max) + s(swi_range) + 
#>     s(temperature_seasonality) + s(humidity_range) + s(topo_elevation) + 
#>     s(soil_sand) + s(topo_diversity) + s(topo_slope)
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 0.387803   0.002669   145.3   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>                              edf Ref.df      F p-value    
#> s(rainfall_mean)           8.179  8.805 15.468 < 2e-16 ***
#> s(soil_temperature_max)    7.723  8.584 35.040 < 2e-16 ***
#> s(swi_range)               4.688  5.810  7.023 7.9e-07 ***
#> s(temperature_seasonality) 1.000  1.000 40.832 < 2e-16 ***
#> s(humidity_range)          1.770  2.227  2.544  0.0669 .  
#> s(topo_elevation)          5.099  6.205 11.725 < 2e-16 ***
#> s(soil_sand)               2.548  3.269  1.694  0.1636    
#> s(topo_diversity)          3.086  3.984  1.565  0.1883    
#> s(topo_slope)              1.000  1.001  0.208  0.6489    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.902   Deviance explained = 90.8%
#> GCV = 0.0046196  Scale est. = 0.0043463  n = 610
```

### Integration with `tidymodels`

The function \[step_collinear()\] wraps \[collinear()\] to facilitate
its usage in `tidymodels` recipes. This integration enables automated
multicollinearity filtering within cross-validation and hyperparameter
tuning workflows. Please notice that `step_collinear()` does not perform
target encoding, as combining this functionality with multicollinearity
filtering does not fit well with how `recipes` works.

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
vi_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

#create and fit workflow
vi_workflow <- workflows::workflow() |>
  workflows::add_recipe(vi_recipe) |>
  workflows::add_model(vi_model) |>
  workflows::fit(data = vi_smol)

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
#>             (Intercept)            rainfall_mean     soil_temperature_max  
#>               7.125e-01                2.888e-05               -1.155e-02  
#>               swi_range  temperature_seasonality           humidity_range  
#>               4.306e-03               -1.106e-04               -3.055e-03  
#>          topo_elevation                soil_sand           topo_diversity  
#>              -4.405e-05               -1.703e-04                1.728e-03  
#>              topo_slope  
#>               7.480e-04
```

## Citation

If you find this package useful, please cite it as:

*Blas M. Benito (2025). collinear: R Package for Seamless
Multicollinearity Management. Version 3.0.0. doi:
10.5281/zenodo.10039489*

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
