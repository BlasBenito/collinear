# Find valid numeric, categorical, and logical variables in a dataframe

Returns a list with the names of the valid numeric, categorical, and
logical variables in a modelling dataframe.

## Usage

``` r
identify_valid_variables(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  decimals = 4,
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- responses:

  (optional; character, character vector, or NULL) Name of one or
  several response variables in `df`. Default: NULL.

- predictors:

  (required, character vector) Names of the predictors to identify.
  Default: NULL

- decimals:

  (required, integer) Number of decimal places for the zero variance
  test. Smaller numbers will increase the number of variables detected
  as near-zero variance. Recommended values will depend on the range of
  the numeric variables in 'df'. Default: 4

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

list

- `numeric`: character vector of numeric predictors.

- `categorical`: character vector of categorical (character and factor)
  predictors.

- `logical`: character vector of logical predictors.

## See also

Other data_types:
[`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md),
[`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md),
[`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md),
[`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md),
[`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol, vi_predictors)

x <- identify_valid_variables(
  df = vi_smol,
  predictors = vi_predictors
)

x
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
#> [47] "humidity_range"            
#> 
#> $categorical
#>  [1] "koppen_zone"        "koppen_group"       "koppen_description"
#>  [4] "soil_type"          "biogeo_ecoregion"   "biogeo_biome"      
#>  [7] "biogeo_realm"       "country_name"       "continent"         
#> [10] "region"             "subregion"         
#> 
```
