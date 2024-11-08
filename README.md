
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Development badges 
# collinear <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>
-->

# collinear: R Package for Seamless Multicollinearity Management

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

Version 2.0.0 of `collinear` includes changes that may disrupt existing
workflows, and results from previous versions may not be reproducible
due to enhancements in the automated selection algorithms. Please refer
to the Changelog for details.

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

These methods are combined in the function `collinear()`, which serves
as single entry point for most of the functionalities in the package.
The article [How It
Works](https://blasbenito.github.io/collinear/articles/how_it_works.html)
explains how `collinear()` works in detail.

## Citation

If you find this package useful, please cite it as:

*Blas M. Benito (2024). collinear: R Package for Seamless
Multicollinearity Management. Version 2.0.0. doi:
10.5281/zenodo.10039489*

## Main Improvements in Version 2.0.0

1.  **Expanded Functionality**: Functions `collinear()` and
    `preference_order()` support both categorical and numeric responses
    and predictors, and can handle several responses at once.
2.  **Robust Selection Algorithms**: Enhanced selection in
    `vif_select()` and `cor_select()`.
3.  **Enhanced Functionality to Rank Predictors**: New functions to
    compute association between response and predictors covering most
    use-cases, and automated function selection depending on data
    features.
4.  **Simplified Target Encoding**: Streamlined and parallelized for
    better efficiency, and new default is “loo” (leave-one-out).
5.  **Parallelization and Progress Bars**: Utilizes `future` and
    `progressr` for enhanced performance and user experience.

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
  ref = "archive_v1.1.1"
  )
```

## Getting Started

The function `collinear()` provides all tools required for a fully
fledged multicollinearity filtering workflow. The code below shows a
small example workflow.

``` r
#parallelization setup
future::plan(
  future::multisession,
  workers = parallelly::availableCores() - 1
  )

#progress bar (does not work in Rmarkdown)
#progressr::handlers(global = TRUE)

#example data frame
df <- collinear::vi[1:5000, ]

#there are many NA cases in this data frame
sum(is.na(df))
#> [1] 3391
```

``` r
#numeric and categorical predictors
predictors <- collinear::vi_predictors

collinear::identify_predictors(
  df = df,
  predictors = predictors
)
#> $numeric
#>  [1] "topo_slope"                 "topo_diversity"            
#>  [3] "topo_elevation"             "swi_mean"                  
#>  [5] "swi_max"                    "swi_min"                   
#>  [7] "swi_range"                  "soil_temperature_mean"     
#>  [9] "soil_temperature_max"       "soil_temperature_min"      
#> [11] "soil_temperature_range"     "soil_sand"                 
#> [13] "soil_clay"                  "soil_silt"                 
#> [15] "soil_ph"                    "soil_soc"                  
#> [17] "soil_nitrogen"              "solar_rad_mean"            
#> [19] "solar_rad_max"              "solar_rad_min"             
#> [21] "solar_rad_range"            "growing_season_length"     
#> [23] "growing_season_temperature" "growing_season_rainfall"   
#> [25] "growing_degree_days"        "temperature_mean"          
#> [27] "temperature_max"            "temperature_min"           
#> [29] "temperature_range"          "temperature_seasonality"   
#> [31] "rainfall_mean"              "rainfall_min"              
#> [33] "rainfall_max"               "rainfall_range"            
#> [35] "evapotranspiration_mean"    "evapotranspiration_max"    
#> [37] "evapotranspiration_min"     "evapotranspiration_range"  
#> [39] "cloud_cover_mean"           "cloud_cover_max"           
#> [41] "cloud_cover_min"            "cloud_cover_range"         
#> [43] "aridity_index"              "humidity_mean"             
#> [45] "humidity_max"               "humidity_min"              
#> [47] "humidity_range"             "country_population"        
#> [49] "country_gdp"               
#> 
#> $categorical
#>  [1] "koppen_zone"        "koppen_group"       "koppen_description"
#>  [4] "soil_type"          "biogeo_ecoregion"   "biogeo_biome"      
#>  [7] "biogeo_realm"       "country_name"       "country_income"    
#> [10] "continent"          "region"             "subregion"
```

``` r
#multicollinearity filtering
selection <- collinear::collinear(
  df = df,
  response = c(
    "vi_numeric",    #numeric response
    "vi_categorical" #categorical response
    ),
  predictors = predictors,
  max_cor = 0.75,
  max_vif = 5,
  quiet = TRUE
)
```

The output is a named list of vectors with selected predictor names when
more than one response is provided, and a character vector otherwise.

``` r
selection
#> $vi_numeric
#>  [1] "growing_season_length"  "soil_temperature_max"   "soil_temperature_range"
#>  [4] "solar_rad_max"          "rainfall_max"           "subregion"             
#>  [7] "biogeo_realm"           "swi_range"              "rainfall_min"          
#> [10] "soil_nitrogen"          "continent"              "cloud_cover_range"     
#> [13] "topo_diversity"        
#> attr(,"validated")
#> [1] TRUE
#> attr(,"response")
#> [1] "vi_numeric"
#> 
#> $vi_categorical
#>  [1] "rainfall_mean"        "swi_mean"             "soil_temperature_max"
#>  [4] "soil_type"            "humidity_max"         "solar_rad_max"       
#>  [7] "country_gdp"          "swi_range"            "rainfall_range"      
#> [10] "country_population"   "soil_soc"             "region"              
#> [13] "country_income"       "topo_diversity"       "topo_slope"          
#> attr(,"validated")
#> [1] TRUE
#> attr(,"response")
#> [1] "vi_categorical"
```

The output of `collinear()` can be easily converted into model formulas.

``` r
formulas <- collinear::model_formula(
  predictors = selection
)

formulas
#> $vi_numeric
#> vi_numeric ~ growing_season_length + soil_temperature_max + soil_temperature_range + 
#>     solar_rad_max + rainfall_max + subregion + biogeo_realm + 
#>     swi_range + rainfall_min + soil_nitrogen + continent + cloud_cover_range + 
#>     topo_diversity
#> <environment: 0x5afafd703c68>
#> 
#> $vi_categorical
#> vi_categorical ~ rainfall_mean + swi_mean + soil_temperature_max + 
#>     soil_type + humidity_max + solar_rad_max + country_gdp + 
#>     swi_range + rainfall_range + country_population + soil_soc + 
#>     region + country_income + topo_diversity + topo_slope
#> <environment: 0x5afafd703c68>
```

These formulas can be used to fit models right away.

``` r
#linear model
m_vi_numeric <- stats::glm(
  formula = formulas[["vi_numeric"]], 
  data = df,
  na.action = na.omit
  )

#random forest model
m_vi_categorical <- ranger::ranger(
  formula = formulas[["vi_categorical"]],
  data = na.omit(df)
)
```

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
