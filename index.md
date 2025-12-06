# `collinear` Automated Multicollinearity Management

## Summary

The R package `collinear` provides a comprehensive toolkit for smart
multicollinearity management in datasets with mixed variable types. The
main function,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
integrates five core components:

- [**Target
  Encoding**](https://blasbenito.github.io/collinear/articles/target_encoding.md)
  (function
  [target_encoding_lab()](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)):
  Transparently converts categorical predictors to numeric when
  required, enabling VIF and correlation analysis across mixed data
  types.

- [**Intelligent Predictor
  Ranking**](https://blasbenito.github.io/collinear/articles/intelligent_predictor_ranking.md)
  (function
  [preference_order()](https://blasbenito.github.io/collinear/reference/preference_order.md)):
  Prioritizes predictors by their univariate association with the
  response to ensure that the most relevant ones are retained during
  filtering.

- [**Unified Correlation
  Framework**](https://blasbenito.github.io/collinear/articles/unified_correlation_framework.md)
  (function
  [cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.md)):
  Computes pairwise correlations between any variable types using
  Pearson correlation (numeric-numeric), target encoding
  (numeric-categorical), and Cramer’s V (categorical-categorical) within
  a single, consistent workflow.

- [**Adaptive Filtering
  Thresholds**](https://blasbenito.github.io/collinear/articles/adaptive_filtering_thresholds.md)
  (function
  [collinear()](https://blasbenito.github.io/collinear/reference/collinear.md)):
  Automatically configures correlation and VIF thresholds based on each
  dataset’s correlation structure, eliminating guesswork while allowing
  manual override.

- [**Dual Filtering
  Strategy**](https://blasbenito.github.io/collinear/articles/dual_filtering_strategy.md)
  (function
  [collinear_select()](https://blasbenito.github.io/collinear/reference/collinear_select.md)):
  Combines pairwise correlation and Variance Inflation Factor filtering
  while considering predictor rankings to manage multicollinearity while
  maximizing the predictive power of the resulting selection of
  predictors.

These methods, except target encoding are also fully integrated into the
`tidymodels` implementation
[step_collinear()](https://blasbenito.github.io/collinear/reference/step_collinear.md).

The package also provides diagnostic functions
([cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.md),
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.md),
[collinear_stats()](https://blasbenito.github.io/collinear/reference/collinear_stats.md))
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
[vi_smol](https://blasbenito.github.io/collinear/reference/vi_smol.md)
has several response variables and a large set of predictors. Here we
focus on the numeric response `vi_numeric`, and the complete set of
numeric and categorical predictors, stored in the vector
[vi_predictors](https://blasbenito.github.io/collinear/reference/vi_predictors.md).

``` r
data(vi_smol, vi_predictors)
nrow(vi_smol)
#> [1] 610
length(vi_predictors)
#> [1] 58
```

### Multicollinearity Analysis

The package provides several functions to assess multicollinearity.

The functions
[cor_df()](https://blasbenito.github.io/collinear/reference/cor_df.md)
and
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.md)
generate dataframes with the pairwise correlations and VIF scores of the
predictors, while the functions
[cor_stats()](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[vif_stats()](https://blasbenito.github.io/collinear/reference/vif_stats.md)
(used below) and
[collinear_stats()](https://blasbenito.github.io/collinear/reference/collinear_stats.md)
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
#>   method     statistic     value
#> 1    vif             n   58.0000
#> 2    vif       minimum -194.5080
#> 3    vif quantile_0.05  -11.2993
#> 4    vif quantile_0.25    0.4528
#> 5    vif          mean   67.8998
#> 6    vif        median   25.2999
#> 7    vif quantile_0.75   90.3970
#> 8    vif quantile_0.95  316.6229
#> 9    vif       maximum  629.6362
```

The quantile 0.75 returned by
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)
indicates that 25% of the predictors have a VIF score higher than 6.5.
This suggests substantial redundancy in the set of predictors.

### Multicollinearity Filtering

To reduce multicollinearity in `vi_smol` we apply
[collinear()](https://blasbenito.github.io/collinear/reference/collinear.md)
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
#> collinear::collinear(): setting 'max_cor' to 0.6842.
#> 
#> collinear::collinear(): setting 'max_vif' to 6.5269.
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
#>  - topo_diversity
#>  - topo_slope
```

The function returns an object of class `collinear_output`, that has its
own [`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods.

``` r
x
#> Result
#> ===================
#>  - response: vi_numeric
#>    --------------------
#> 
#>  + df:
#>    - rows: 610
#>    - cols: 14
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
#>    - ... (8 ommited)
#> 
#>  + formulas:
#>    - linear: vi_numeric ~ rainfall_mean + swi_mean + evapotranspiration_max + evapotranspiration_range + swi_min + ... (8 terms omitted) 
#>    - smooth: vi_numeric ~ s(rainfall_mean) + s(swi_mean) + s(evapotranspiration_max) + s(evapotranspiration_range) + s(swi_min) + ... (8 terms omitted)
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
#> [13] "topo_diversity"           "topo_slope"
```

The object `preference_order` contains the ranking of predictors. It is
computed by the function
[preference_order()](https://blasbenito.github.io/collinear/reference/preference_order.md)
by assessing the association between the response and the predictors. In
this case, it fits univariate models between the response and each
predictor using
[f_numeric_rf](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md),
and returns the R-squared of the observations vs the model predictions.
This ranking ensures that the most important predictors are protected
during multicollinearity filtering.

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
#>  [1] "rainfall_mean"            "swi_mean"                
#>  [3] "evapotranspiration_max"   "evapotranspiration_range"
#>  [5] "swi_min"                  "soil_soc"                
#>  [7] "humidity_range"           "topo_elevation"          
#>  [9] "cloud_cover_range"        "continent"               
#> [11] "soil_sand"                "topo_diversity"          
#> [13] "topo_slope"              
#> attr(,"validated")
#> [1] TRUE
```

We can check that this selection of predictors shows low
multicollinearity by running the function
[vif_df()](https://blasbenito.github.io/collinear/reference/vif_df.md)
on them.

``` r
collinear::vif_df(
  df = x$vi_numeric$df,
  predictors = x$vi_numeric$selection
)
#>        vif                predictor
#> 1   4.7203        cloud_cover_range
#> 2   3.3447                continent
#> 3   3.1112   evapotranspiration_max
#> 4   3.0627 evapotranspiration_range
#> 5   2.3856           humidity_range
#> 6   2.0747            rainfall_mean
#> 7   1.9928                soil_sand
#> 8   1.7690                 soil_soc
#> 9   1.6346                 swi_mean
#> 10  1.5984                  swi_min
#> 11  1.3531           topo_diversity
#> 12  0.9320           topo_elevation
#> 13 -0.2235               topo_slope
```

All VIF scores are below 2.5,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
did a good job here!

Finally,
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
also returns modeling formulas to help kick start exploratory modelling.

``` r
x$vi_numeric$formulas
#> $linear
#> vi_numeric ~ rainfall_mean + swi_mean + evapotranspiration_max + 
#>     evapotranspiration_range + swi_min + soil_soc + humidity_range + 
#>     topo_elevation + cloud_cover_range + continent + soil_sand + 
#>     topo_diversity + topo_slope
#> <environment: 0x58a529a52b68>
#> 
#> $smooth
#> vi_numeric ~ s(rainfall_mean) + s(swi_mean) + s(evapotranspiration_max) + 
#>     s(evapotranspiration_range) + s(swi_min) + s(soil_soc) + 
#>     s(humidity_range) + s(topo_elevation) + s(cloud_cover_range) + 
#>     continent + s(soil_sand) + s(topo_diversity) + s(topo_slope)
#> <environment: 0x58a529a52b68>
```

The function returns linear formulas for numeric outcomes, and
classification formulas for categorical outcomes.

### Model Fitting

The output of
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
can be used right away to fit exploratory models, as shown below.

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
#> -0.42639 -0.04274  0.00107  0.04804  0.24228 
#> 
#> Coefficients:
#>                            Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)               3.027e-01  4.557e-02   6.644 6.97e-11 ***
#> rainfall_mean             4.183e-05  7.152e-06   5.849 8.22e-09 ***
#> swi_mean                  6.997e-03  4.415e-04  15.850  < 2e-16 ***
#> evapotranspiration_max   -1.206e-03  1.694e-04  -7.117 3.22e-12 ***
#> evapotranspiration_range -1.763e-04  1.396e-04  -1.263 0.207122    
#> swi_min                  -2.473e-03  6.162e-04  -4.013 6.77e-05 ***
#> soil_soc                 -4.845e-04  1.944e-04  -2.492 0.012964 *  
#> humidity_range           -1.042e-03  6.301e-04  -1.654 0.098731 .  
#> topo_elevation           -3.991e-05  7.280e-06  -5.481 6.27e-08 ***
#> cloud_cover_range         5.109e-04  3.588e-04   1.424 0.155005    
#> continentAsia            -2.847e-02  1.227e-02  -2.320 0.020687 *  
#> continentEurope           3.893e-02  1.895e-02   2.054 0.040412 *  
#> continentNorth America    6.304e-02  1.525e-02   4.134 4.08e-05 ***
#> continentOceania          4.555e-02  1.478e-02   3.083 0.002146 ** 
#> continentSouth America    5.547e-02  1.242e-02   4.466 9.54e-06 ***
#> soil_sand                 5.123e-04  2.821e-04   1.816 0.069887 .  
#> topo_diversity            3.318e-03  8.973e-04   3.698 0.000238 ***
#> topo_slope                4.209e-03  1.392e-03   3.024 0.002600 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.08237 on 589 degrees of freedom
#>   (3 observations deleted due to missingness)
#> Multiple R-squared:  0.8515, Adjusted R-squared:  0.8473 
#> F-statistic: 198.7 on 17 and 589 DF,  p-value: < 2.2e-16
```

### Integration with `tidymodels`

The function
[step_collinear()](https://blasbenito.github.io/collinear/reference/step_collinear.md)
wraps
[collinear()](https://blasbenito.github.io/collinear/reference/collinear.md)
to facilitate its usage in `tidymodels` recipes. Please notice that
[`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md)
does not perform target encoding, as combining this functionality with
multicollinearity filtering does not fit well with how `recipes` works.

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
#>                3.027e-01                 4.183e-05                 6.997e-03  
#>   evapotranspiration_max  evapotranspiration_range                   swi_min  
#>               -1.206e-03                -1.763e-04                -2.473e-03  
#>                 soil_soc            humidity_range            topo_elevation  
#>               -4.845e-04                -1.042e-03                -3.991e-05  
#>        cloud_cover_range             continentAsia           continentEurope  
#>                5.109e-04                -2.847e-02                 3.893e-02  
#>   continentNorth America          continentOceania    continentSouth America  
#>                6.304e-02                 4.555e-02                 5.547e-02  
#>                soil_sand            topo_diversity                topo_slope  
#>                5.123e-04                 3.318e-03                 4.209e-03
```

## Citation

If you find this package useful, please cite it as:

*Blas M. Benito (2025). collinear: R Package for Seamless
Multicollinearity Management. Version 3.0.0. doi:
10.5281/zenodo.10039489*

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
