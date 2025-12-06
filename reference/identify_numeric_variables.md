# Find valid numeric variables in a dataframe

Identifies valid numeric variables and ignores those with constant
values.

## Usage

``` r
identify_numeric_variables(
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

list:

- `valid`: character vector with valid numeric predictor names.

- `invalid`: character vector with invalid numeric predictor names due
  to near-zero variance.

## See also

Other data_types:
[`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md),
[`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md),
[`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md),
[`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md),
[`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol, vi_predictors)

x <- identify_numeric_variables(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors
)

#valid numeric predictors
x$valid
#>  [1] "vi_numeric"                 "topo_slope"                
#>  [3] "topo_diversity"             "topo_elevation"            
#>  [5] "swi_mean"                   "swi_max"                   
#>  [7] "swi_min"                    "swi_range"                 
#>  [9] "soil_temperature_mean"      "soil_temperature_max"      
#> [11] "soil_temperature_min"       "soil_temperature_range"    
#> [13] "soil_sand"                  "soil_clay"                 
#> [15] "soil_silt"                  "soil_ph"                   
#> [17] "soil_soc"                   "soil_nitrogen"             
#> [19] "solar_rad_mean"             "solar_rad_max"             
#> [21] "solar_rad_min"              "solar_rad_range"           
#> [23] "growing_season_length"      "growing_season_temperature"
#> [25] "growing_season_rainfall"    "growing_degree_days"       
#> [27] "temperature_mean"           "temperature_max"           
#> [29] "temperature_min"            "temperature_range"         
#> [31] "temperature_seasonality"    "rainfall_mean"             
#> [33] "rainfall_min"               "rainfall_max"              
#> [35] "rainfall_range"             "evapotranspiration_mean"   
#> [37] "evapotranspiration_max"     "evapotranspiration_min"    
#> [39] "evapotranspiration_range"   "cloud_cover_mean"          
#> [41] "cloud_cover_max"            "cloud_cover_min"           
#> [43] "cloud_cover_range"          "aridity_index"             
#> [45] "humidity_mean"              "humidity_max"              
#> [47] "humidity_min"               "humidity_range"            

#invalid due to zero variance (none here)
x$invalid
#> NULL
```
