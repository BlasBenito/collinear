
<!-- README.md is generated from README.Rmd. Please edit that file -->

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

These methods are combined in the function `collinear()`, and as
individual functions in `target_encoding_lab()`, `preference_order()`,
`cor_select()`, and `vif_select()`.

The article [How It
Works](https://blasbenito.github.io/collinear/articles/how_it_works/how_it_works.html)
explains these functions in detail.

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

The function `collinear()` provides access to the key functionalities of
the package.

The code below shows a example call to `collinear()` with two responses
and mixed predictor types.

``` r
#parallelization setup
future::plan(
  future::multisession,
  workers = parallelly::availableCores() - 1
  )

#multicollinearity filtering
selection <- collinear::collinear(
  df = collinear::vi,
  response = c(
    "vi_numeric",
    "vi_categorical"
    ),
  predictors = collinear::vi_predictors,
  cor_max = 0.75,
  vif_max = 5
)
#> 
#> collinear::collinear(): processing response 'vi_numeric'.
#> ---------------------------------------------------------------
#> 
#> collinear::target_encoding_lab(): using response 'vi_numeric' to encode categorical predictors:
#>  - koppen_zone
#>  - koppen_group
#>  - koppen_description
#>  - soil_type
#>  - biogeo_ecoregion
#>  - biogeo_biome
#>  - biogeo_realm
#>  - country_name
#>  - country_income
#>  - continent
#>  - region
#>  - subregion
#> 
#> collinear::preference_order(): ranking predictors for response 'vi_numeric'.
#> 
#> collinear::f_auto(): selected function: 'f_r2_pearson()'.
#> 
#> collinear::cor_select(): computing pairwise correlation matrix.
#> 
#> collinear::cor_select(): selected predictors: 
#>  - biogeo_ecoregion
#>  - cloud_cover_mean
#>  - swi_max
#>  - rainfall_max
#>  - aridity_index
#>  - subregion
#>  - biogeo_realm
#>  - evapotranspiration_range
#>  - rainfall_min
#>  - swi_min
#>  - solar_rad_mean
#>  - soil_nitrogen
#>  - continent
#>  - temperature_max
#>  - soil_soc
#>  - temperature_min
#>  - cloud_cover_range
#>  - topo_diversity
#>  - soil_clay
#>  - humidity_range
#>  - country_income
#>  - soil_sand
#>  - topo_elevation
#>  - topo_slope
#>  - country_gdp
#>  - country_population
#> 
#> collinear::vif_select(): selected predictors: 
#>  - biogeo_ecoregion
#>  - cloud_cover_mean
#>  - swi_max
#>  - rainfall_max
#>  - aridity_index
#>  - subregion
#>  - biogeo_realm
#>  - evapotranspiration_range
#>  - swi_min
#>  - soil_nitrogen
#>  - continent
#>  - temperature_max
#>  - topo_diversity
#>  - topo_slope
#> 
#> collinear::collinear(): processing response 'vi_categorical'.
#> ---------------------------------------------------------------
#> 
#> collinear::target_encoding_lab(): argument 'response' is not numeric, skipping target-encoding.
#> 
#> collinear::preference_order(): ranking predictors for response 'vi_categorical'.
#> 
#> collinear::f_auto(): selected function: 'f_v_rf_categorical()'.
#> 
#> collinear::cor_select(): computing pairwise correlation matrix.
#> 
#> collinear::cor_select(): selected predictors: 
#>  - rainfall_mean
#>  - koppen_group
#>  - soil_type
#>  - humidity_max
#>  - humidity_min
#>  - evapotranspiration_max
#>  - solar_rad_max
#>  - rainfall_range
#>  - swi_range
#>  - subregion
#>  - rainfall_min
#>  - soil_soc
#>  - biogeo_biome
#>  - soil_nitrogen
#>  - cloud_cover_range
#>  - humidity_range
#>  - soil_sand
#>  - soil_clay
#>  - topo_diversity
#>  - topo_slope
#>  - topo_elevation
#> 
#> collinear::vif_select(): selected predictors: 
#>  - rainfall_mean
#>  - humidity_max
#>  - humidity_min
#>  - evapotranspiration_max
#>  - solar_rad_max
#>  - swi_range
#>  - rainfall_min
#>  - soil_soc
#>  - soil_nitrogen
#>  - soil_sand
#>  - soil_clay
#>  - topo_diversity
#>  - topo_slope
#>  - topo_elevation
#> 
#> collinear::collinear(): selected predictors: 
#>  - rainfall_mean
#>  - koppen_group
#>  - soil_type
#>  - humidity_max
#>  - humidity_min
#>  - evapotranspiration_max
#>  - solar_rad_max
#>  - swi_range
#>  - subregion
#>  - rainfall_min
#>  - soil_soc
#>  - biogeo_biome
#>  - soil_nitrogen
#>  - soil_sand
#>  - soil_clay
#>  - topo_diversity
#>  - topo_slope
#>  - topo_elevation
```

The output is a named list of vectors with predictor names when more
than one response is provided, and a character vector otherwise.

``` r
selection
#> $vi_numeric
#>  [1] "biogeo_ecoregion"         "cloud_cover_mean"        
#>  [3] "swi_max"                  "rainfall_max"            
#>  [5] "aridity_index"            "subregion"               
#>  [7] "biogeo_realm"             "evapotranspiration_range"
#>  [9] "swi_min"                  "soil_nitrogen"           
#> [11] "continent"                "temperature_max"         
#> [13] "topo_diversity"           "topo_slope"              
#> attr(,"validated")
#> [1] TRUE
#> 
#> $vi_categorical
#>  [1] "rainfall_mean"          "koppen_group"           "soil_type"             
#>  [4] "humidity_max"           "humidity_min"           "evapotranspiration_max"
#>  [7] "solar_rad_max"          "swi_range"              "subregion"             
#> [10] "rainfall_min"           "soil_soc"               "biogeo_biome"          
#> [13] "soil_nitrogen"          "soil_sand"              "soil_clay"             
#> [16] "topo_diversity"         "topo_slope"             "topo_elevation"        
#> attr(,"validated")
#> [1] TRUE
```

## Getting help

If you encounter bugs or issues with the documentation, please [file a
issue on GitHub](https://github.com/BlasBenito/collinear/issues).
