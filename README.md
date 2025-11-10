
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

## Warning

Version 3.0.0 of `collinear` includes changes that may disrupt existing
workflows, and results from previous versions may not be reproducible
due to enhancements in the automated selection algorithms.

## Summary

[Multicollinearity hinders the
interpretability](https://www.blasbenito.com/post/multicollinearity-model-interpretability/)
of linear and machine learning models.

The `collinear` package combines four methods for easy management of
multicollinearity in modelling data frames with numeric and categorical
variables:

- **Target Encoding**: Transforms categorical predictors to numeric
  using a numeric response as reference.
- **Preference Order**: Ranks predictors by their association with a
  response variable to preserve important ones in multicollinearity
  filtering.
- **Pairwise Correlation Filtering**: Automated multicollinearity
  filtering of numeric and categorical predictors based on pairwise
  correlations.
- **Variance Inflation Factor Filtering**: Automated multicollinearity
  filtering of numeric predictors based on Variance Inflation Factors.

These four methods are combined in the functions `collinear()` and
`collinear_auto()`, which serves as single entry point for most of the
functionalities in the package. The article [How It
Works](https://blasbenito.github.io/collinear/articles/how_it_works.html)
explains how these functions work in detail.

## Citation

If you find this package useful, please cite it as:

*Blas M. Benito (2025). collinear: R Package for Seamless
Multicollinearity Management. Version 3.0.0. doi:
10.5281/zenodo.10039489*

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

Previous versions are in the “archive_xxx” branches of the GitHub
repository.

``` r
remotes::install_github(
  repo = "blasbenito/collinear", 
  ref = "archive_v2.0.0"
  )
```

## Getting Started

The function `collinear_auto()` has the lowest entry barrier, as it
handles multicollinearity management all by itself.

``` r
library(collinear)
library(future)

#parallelization setup
#only worth it for large data with categorical predictors
future::plan(
  future::multisession,
  workers = future::availableCores() - 1
  )

#progress bar (does not work in Rmarkdown)
#progressr::handlers(global = TRUE)

#multicollinearity filtering
#two responses
#numeric and categorical predictors
x <- collinear::collinear_auto(
  df = collinear::vi_smol,
  response = c(
    "vi_numeric",    #numeric response
    "vi_categorical" #categorical response
    ),
  predictors = collinear::vi_predictors,
  quiet = TRUE #set to FALSE to see all messages
)
```

The output is a list of class `collinear_output`, that has a proper
`print()` method.

``` r
x
#> Results
#> ===================
#> 
#>  - response: vi_numeric
#>    --------------------
#> 
#>  + selection:
#>    - growing_season_length
#>    - temperature_seasonality
#>    - swi_min
#>    - continent
#>    - rainfall_min
#>    - ... (7 ommited)
#> 
#>  + formulas:
#>    - linear: vi_numeric ~ growing_season_length + temperature_seasonality + swi_min + continent + rainfall_min + ... (7 terms omitted) 
#>    - smooth: vi_numeric ~ s(growing_season_length) + s(temperature_seasonality) + s(swi_min) + continent + s(rainfall_min) + ... (7 terms omitted) 
#> 
#>  + df:
#>    - rows: 610
#>    - cols: 13
#> 
#>  + preference order:
#>    + df:
#>      - rows: 61
#>      - cols: 6
#>    + preference:
#>      - growing_season_length
#>      - koppen_zone
#>      - soil_ph
#>      - koppen_description
#>      - swi_mean
#>    + f: f_numeric_glm
#> 
#> 
#>  - response: vi_categorical
#>    ------------------------
#> 
#>  + selection:
#>    - rainfall_mean
#>    - soil_temperature_max
#>    - swi_range
#>    - country_gdp
#>    - country_population
#>    - ... (7 ommited)
#> 
#>  - formula:
#>    - classification: vi_categorical ~ rainfall_mean + soil_temperature_max + swi_range + country_gdp + country_population + ... (7 terms omitted) 
#> 
#>  + df:
#>    - rows: 610
#>    - cols: 13
#> 
#>  + preference order:
#>    + df:
#>      - rows: 61
#>      - cols: 6
#>    + preference:
#>      - rainfall_mean
#>      - growing_season_rainfall
#>      - growing_season_length
#>      - aridity_index
#>      - swi_mean
#>    + f: f_categorical_rf
```

The function `vif_df()` provides the simplest way to check that the
variable selections returned by `collinear_auto()` show low
multicollinearity.

``` r
vif_df(
  df = x$vi_numeric$df,
  predictors = x$vi_numeric$selection
)
#>       vif               predictor
#> 1  2.4659       cloud_cover_range
#> 2  1.8341               continent
#> 3  1.7570          country_income
#> 4  1.6957   growing_season_length
#> 5  1.6780          humidity_range
#> 6  1.6665            rainfall_min
#> 7  1.6139               soil_clay
#> 8  1.4381                 swi_min
#> 9  1.3525 temperature_seasonality
#> 10 1.3502          topo_diversity
#> 11 1.2706          topo_elevation
#> 12 1.0510              topo_slope
```

``` r
vif_df(
  df = x$vi_categorical$df,
  predictors = x$vi_categorical$selection
)
#>       vif            predictor
#> 1  2.1982          country_gdp
#> 2  1.9782       country_income
#> 3  1.8466   country_population
#> 4  1.8244       humidity_range
#> 5  1.8215        rainfall_mean
#> 6  1.7090            soil_sand
#> 7  1.5910 soil_temperature_max
#> 8  1.5287            swi_range
#> 9  1.4617      temperature_min
#> 10 1.4566       topo_diversity
#> 11 1.3260       topo_elevation
#> 12 1.1720           topo_slope
```

The output of `preference_order()` computed internally by
`collinear_auto()` and used to preserve important predictors is
available in the function’s output. For example, for the response
`vi_numeric`, the R-squared of the observations vs the predictors of a
GLM fitted on the response against each predictor was used to ran the
predictors.

``` r
x$vi_numeric$preference_order
#>      response                  predictor             f    metric  score rank
#> 1  vi_numeric      growing_season_length f_numeric_glm R-squared 0.8149    1
#> 2  vi_numeric                koppen_zone f_numeric_glm R-squared 0.7972    2
#> 3  vi_numeric                    soil_ph f_numeric_glm R-squared 0.7683    3
#> 4  vi_numeric         koppen_description f_numeric_glm R-squared 0.7657    4
#> 5  vi_numeric                   swi_mean f_numeric_glm R-squared 0.7182    5
#> 6  vi_numeric               koppen_group f_numeric_glm R-squared 0.7109    6
#> 7  vi_numeric              humidity_mean f_numeric_glm R-squared 0.6920    7
#> 8  vi_numeric                  soil_type f_numeric_glm R-squared 0.6189    8
#> 9  vi_numeric           cloud_cover_mean f_numeric_glm R-squared 0.5959    9
#> 10 vi_numeric           biogeo_ecoregion f_numeric_glm R-squared 0.5815   10
#> 11 vi_numeric               country_name f_numeric_glm R-squared 0.5786   11
#> 12 vi_numeric               humidity_max f_numeric_glm R-squared 0.5681   12
#> 13 vi_numeric            cloud_cover_max f_numeric_glm R-squared 0.5662   13
#> 14 vi_numeric               humidity_min f_numeric_glm R-squared 0.5630   14
#> 15 vi_numeric                    swi_max f_numeric_glm R-squared 0.5549   15
#> 16 vi_numeric     soil_temperature_range f_numeric_glm R-squared 0.5501   16
#> 17 vi_numeric       soil_temperature_max f_numeric_glm R-squared 0.5462   17
#> 18 vi_numeric               biogeo_biome f_numeric_glm R-squared 0.5435   18
#> 19 vi_numeric              rainfall_mean f_numeric_glm R-squared 0.5354   19
#> 20 vi_numeric                  subregion f_numeric_glm R-squared 0.5079   20
#> 21 vi_numeric    growing_season_rainfall f_numeric_glm R-squared 0.4748   21
#> 22 vi_numeric              solar_rad_max f_numeric_glm R-squared 0.4669   22
#> 23 vi_numeric               rainfall_max f_numeric_glm R-squared 0.4631   23
#> 24 vi_numeric     evapotranspiration_max f_numeric_glm R-squared 0.4590   24
#> 25 vi_numeric   evapotranspiration_range f_numeric_glm R-squared 0.4489   25
#> 26 vi_numeric              aridity_index f_numeric_glm R-squared 0.4347   26
#> 27 vi_numeric                  swi_range f_numeric_glm R-squared 0.4225   27
#> 28 vi_numeric               biogeo_realm f_numeric_glm R-squared 0.4076   28
#> 29 vi_numeric            cloud_cover_min f_numeric_glm R-squared 0.3769   29
#> 30 vi_numeric          temperature_range f_numeric_glm R-squared 0.3752   30
#> 31 vi_numeric             rainfall_range f_numeric_glm R-squared 0.3501   31
#> 32 vi_numeric    temperature_seasonality f_numeric_glm R-squared 0.2579   32
#> 33 vi_numeric                    swi_min f_numeric_glm R-squared 0.2130   33
#> 34 vi_numeric                  continent f_numeric_glm R-squared 0.2046   34
#> 35 vi_numeric             solar_rad_mean f_numeric_glm R-squared 0.1965   35
#> 36 vi_numeric               rainfall_min f_numeric_glm R-squared 0.1865   36
#> 37 vi_numeric                     region f_numeric_glm R-squared 0.1796   37
#> 38 vi_numeric              soil_nitrogen f_numeric_glm R-squared 0.1759   38
#> 39 vi_numeric            temperature_max f_numeric_glm R-squared 0.1508   39
#> 40 vi_numeric            solar_rad_range f_numeric_glm R-squared 0.1397   40
#> 41 vi_numeric                   soil_soc f_numeric_glm R-squared 0.1365   41
#> 42 vi_numeric    evapotranspiration_mean f_numeric_glm R-squared 0.1292   42
#> 43 vi_numeric          cloud_cover_range f_numeric_glm R-squared 0.1282   43
#> 44 vi_numeric            temperature_min f_numeric_glm R-squared 0.1176   44
#> 45 vi_numeric       soil_temperature_min f_numeric_glm R-squared 0.0972   45
#> 46 vi_numeric                  soil_clay f_numeric_glm R-squared 0.0815   46
#> 47 vi_numeric             topo_diversity f_numeric_glm R-squared 0.0663   47
#> 48 vi_numeric             humidity_range f_numeric_glm R-squared 0.0533   48
#> 49 vi_numeric                  soil_sand f_numeric_glm R-squared 0.0460   49
#> 50 vi_numeric             topo_elevation f_numeric_glm R-squared 0.0351   50
#> 51 vi_numeric             country_income f_numeric_glm R-squared 0.0320   51
#> 52 vi_numeric growing_season_temperature f_numeric_glm R-squared 0.0176   52
#> 53 vi_numeric                 topo_slope f_numeric_glm R-squared 0.0176   53
#> 54 vi_numeric      soil_temperature_mean f_numeric_glm R-squared 0.0151   54
#> 55 vi_numeric                  soil_silt f_numeric_glm R-squared 0.0070   55
#> 56 vi_numeric           temperature_mean f_numeric_glm R-squared 0.0067   56
#> 57 vi_numeric        growing_degree_days f_numeric_glm R-squared 0.0048   57
#> 58 vi_numeric     evapotranspiration_min f_numeric_glm R-squared 0.0034   58
#> 59 vi_numeric                country_gdp f_numeric_glm R-squared 0.0032   59
#> 60 vi_numeric         country_population f_numeric_glm R-squared 0.0007   60
#> 61 vi_numeric              solar_rad_min f_numeric_glm R-squared 0.0001   61
```

We can also use the results of `collinear_output()` to fit exploratory
models. For example, for the response `vi_numeric` we can fit a GLM.

``` r
m <- stats::glm(
  formula = x$vi_numeric$formulas$linear, 
  data = x$vi_numeric$df,
  na.action = na.omit
  )

summary(m)
#> 
#> Call:
#> stats::glm(formula = x$vi_numeric$formulas$linear, data = x$vi_numeric$df, 
#>     na.action = na.omit)
#> 
#> Coefficients:
#>                           Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)              5.666e-02  4.260e-02   1.330 0.183979    
#> growing_season_length    1.040e-03  5.190e-05  20.036  < 2e-16 ***
#> temperature_seasonality -1.163e-04  1.423e-05  -8.176 1.76e-15 ***
#> swi_min                  4.290e-03  4.342e-04   9.881  < 2e-16 ***
#> continent                1.920e-01  3.786e-02   5.071 5.30e-07 ***
#> rainfall_min            -3.102e-05  8.098e-05  -0.383 0.701829    
#> cloud_cover_range        4.555e-04  3.276e-04   1.390 0.164955    
#> soil_clay               -4.830e-04  4.597e-04  -1.051 0.293785    
#> topo_diversity           1.573e-03  8.799e-04   1.787 0.074384 .  
#> humidity_range           3.473e-04  6.156e-04   0.564 0.572813    
#> topo_elevation          -2.105e-05  6.144e-06  -3.427 0.000653 ***
#> country_income           2.919e-02  7.619e-02   0.383 0.701794    
#> topo_slope               2.826e-04  1.333e-03   0.212 0.832194    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for gaussian family taken to be 0.006568831)
#> 
#>     Null deviance: 27.0525  on 609  degrees of freedom
#> Residual deviance:  3.9216  on 597  degrees of freedom
#> AIC: -1319.5
#> 
#> Number of Fisher Scoring iterations: 2
```

For `vi_categorical`, a factor variable, we can fit a classification
model with Random Forest:

``` r
m <- ranger::ranger(
  formula = x$vi_categorical$formulas$classification,
  data = x$vi_categorical$df
)

print(m)
```

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
