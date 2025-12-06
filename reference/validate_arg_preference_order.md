# Check and complete argument `preference_order`

Internal function to validate the argument `preference_order` in
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
and
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).
Predictors not in `preference_order` are ranked from lower to higher sum
of Pearson correlations with all other predictors.

## Usage

``` r
validate_arg_preference_order(
  df = NULL,
  response = NULL,
  predictors = NULL,
  preference_order = NULL,
  quiet = FALSE,
  function_name = NULL,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- response:

  (optional, character string) Name of a numeric response variable in
  `df`. Default: NULL.

- predictors:

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- preference_order:

  (optional; character vector, dataframe from
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md),
  or NULL) Prioritizes predictors to preserve.

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- function_name:

  (optional, character string) Name of the function performing the
  argument check. Default: NULL

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

character vector: ranked variable names

## See also

Other argument_validation:
[`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md),
[`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md),
[`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md),
[`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md),
[`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md),
[`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
[`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md),
[`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md),
[`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md),
[`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md),
[`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)

## Examples

``` r
data(
  vi_smol,
  vi_predictors_numeric
  )

#input arguments must be validated first
df <- validate_arg_df(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  quiet = TRUE
)

response <- validate_arg_responses(
  df = df,
  responses = "vi_numeric"
)

predictors <- validate_arg_predictors(
  df = df,
  response = response,
  predictors = vi_predictors_numeric[1:10]
)



#no preference order
#no response
#ranks predictor from lower to higher multicollinearity
y <- validate_arg_preference_order(
  df = df,
  predictors = predictors,
  preference_order = NULL
)
#> 
#> collinear::validate_arg_preference_order()
#> └── collinear::preference_order(): ranking 10 'predictors' from lower to higher multicollinearity.

y
#>    response             predictor            f        metric  score rank
#> 1      none        topo_elevation stats::cor() 1 - R-squared 0.8184    1
#> 2      none        topo_diversity stats::cor() 1 - R-squared 0.8183    2
#> 3      none            topo_slope stats::cor() 1 - R-squared 0.8001    3
#> 4      none  soil_temperature_min stats::cor() 1 - R-squared 0.7434    4
#> 5      none             swi_range stats::cor() 1 - R-squared 0.7387    5
#> 6      none soil_temperature_mean stats::cor() 1 - R-squared 0.6534    6
#> 7      none              swi_mean stats::cor() 1 - R-squared 0.6272    7
#> 8      none               swi_max stats::cor() 1 - R-squared 0.6259    8
#> 9      none               swi_min stats::cor() 1 - R-squared 0.6170    9
#> 10     none  soil_temperature_max stats::cor() 1 - R-squared 0.6021   10
attributes(y)$validated
#> [1] TRUE


#validate character vector
y <- validate_arg_preference_order(
  df = df,
  predictors = predictors,
  preference_order = c(
    "swi_max",
    "swi_min",
    "swi_deviance" #does not exist
  )
)
#> 
#> collinear::validate_arg_preference_order()
#> └── collinear::preference_order(): ranking 8 'predictors' from lower to higher multicollinearity.

y
#>    response             predictor            f          metric  score rank
#> 1      none               swi_max         <NA> user_preference 1.0000    1
#> 2      none               swi_min         <NA> user_preference 0.0000    2
#> 3      none        topo_diversity stats::cor()   1 - R-squared 0.8049    3
#> 4      none             swi_range stats::cor()   1 - R-squared 0.7981    4
#> 5      none        topo_elevation stats::cor()   1 - R-squared 0.7859    5
#> 6      none            topo_slope stats::cor()   1 - R-squared 0.7855    6
#> 7      none  soil_temperature_min stats::cor()   1 - R-squared 0.7452    7
#> 8      none              swi_mean stats::cor()   1 - R-squared 0.7229    8
#> 9      none soil_temperature_mean stats::cor()   1 - R-squared 0.7057    9
#> 10     none  soil_temperature_max stats::cor()   1 - R-squared 0.6854   10
attributes(y)$validated
#> [1] TRUE

#validate output of preference order
x <- preference_order(
  df = df,
  responses = response,
  predictors = predictors
)
#> 
#> collinear::preference_order()
#> └── collinear::f_auto(): selected function 'f_numeric_glm()' to compute preference order.

x
#>      response             predictor             f    metric  score rank
#> 1  vi_numeric              swi_mean f_numeric_glm R-squared 0.7182    1
#> 2  vi_numeric               swi_max f_numeric_glm R-squared 0.5549    2
#> 3  vi_numeric  soil_temperature_max f_numeric_glm R-squared 0.5462    3
#> 4  vi_numeric             swi_range f_numeric_glm R-squared 0.4225    4
#> 5  vi_numeric               swi_min f_numeric_glm R-squared 0.2130    5
#> 6  vi_numeric  soil_temperature_min f_numeric_glm R-squared 0.0972    6
#> 7  vi_numeric        topo_diversity f_numeric_glm R-squared 0.0663    7
#> 8  vi_numeric        topo_elevation f_numeric_glm R-squared 0.0351    8
#> 9  vi_numeric            topo_slope f_numeric_glm R-squared 0.0176    9
#> 10 vi_numeric soil_temperature_mean f_numeric_glm R-squared 0.0151   10

y <- validate_arg_preference_order(
  df = df,
  response = response,
  predictors = predictors,
  preference_order = x
)

y
#>      response             predictor             f    metric  score rank
#> 1  vi_numeric              swi_mean f_numeric_glm R-squared 0.7182    1
#> 2  vi_numeric               swi_max f_numeric_glm R-squared 0.5549    2
#> 3  vi_numeric  soil_temperature_max f_numeric_glm R-squared 0.5462    3
#> 4  vi_numeric             swi_range f_numeric_glm R-squared 0.4225    4
#> 5  vi_numeric               swi_min f_numeric_glm R-squared 0.2130    5
#> 6  vi_numeric  soil_temperature_min f_numeric_glm R-squared 0.0972    6
#> 7  vi_numeric        topo_diversity f_numeric_glm R-squared 0.0663    7
#> 8  vi_numeric        topo_elevation f_numeric_glm R-squared 0.0351    8
#> 9  vi_numeric            topo_slope f_numeric_glm R-squared 0.0176    9
#> 10 vi_numeric soil_temperature_mean f_numeric_glm R-squared 0.0151   10
attributes(y)$validated
#> [1] TRUE
```
