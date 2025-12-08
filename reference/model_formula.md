# Build model formulas from response and predictors

Generates model formulas from a dataframe, a response name, and a vector
of predictors that can be the output of a multicollinearity management
function such as
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
and the likes. Intended to help fit exploratory models from the result
of a multicollinearity analysis.

The types of formulas it can generate are:

- additive: `y ~ x + z`

- polynomial: `y ~ poly(x, ...) + poly(z, ...)`

- GAM: `y ~ s(x) + s(z)`

- random effect: `y ~ x + (1 \ z)`

## Usage

``` r
model_formula(
  df = NULL,
  response = NULL,
  predictors = NULL,
  term_f = NULL,
  term_args = NULL,
  random_effects = NULL,
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

- response:

  (optional, character string) Name of a response variable in `df`.
  Default: NULL.

- predictors:

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- term_f:

  (optional; string). Name of function to apply to each term in the
  formula, such as "s" for
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html) or any other
  smoothing function, "poly" for
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html). Default: NULL

- term_args:

  (optional; string). Arguments of the function applied to each term.
  For example, for "poly" it can be "degree = 2, raw = TRUE". Default:
  NULL

- random_effects:

  (optional, string or character vector). Names of variables to be used
  as random effects. Each element is added to the final formula as
  `+(1 | random_effect_name)`. Default: NULL

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

list if `predictors` is a list or length of `response` is higher than
one, and character vector otherwise.

## See also

Other modelling_tools:
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md),
[`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md),
[`score_cramer()`](https://blasbenito.github.io/collinear/reference/score_cramer.md),
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)

## Examples

``` r
data(
  vi_smol,
  vi_predictors_numeric
  )

#reduce collinearity
x <- collinear_select(
  df = vi_smol,
  predictors = vi_predictors_numeric
)
#> 
#> collinear::collinear_select()
#> └── collinear::validate_arg_preference_order()
#>     └── collinear::preference_order(): ranking 47 'predictors' from lower to higher multicollinearity.

#additive formula
y <- model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = x
)

y
#> vi_numeric ~ topo_elevation + topo_slope + humidity_range + topo_diversity + 
#>     soil_clay + cloud_cover_range + soil_silt + rainfall_min + 
#>     growing_season_temperature + swi_max + soil_nitrogen + evapotranspiration_range
#> <environment: 0x55fbc3dbc110>

#using a formula in a model
m <- stats::lm(
 formula = y,
 data = vi_smol
 )

summary(m)
#> 
#> Call:
#> stats::lm(formula = y, data = vi_smol)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.43746 -0.06323 -0.00511  0.06252  0.28047 
#> 
#> Coefficients:
#>                              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                 2.765e-01  5.235e-02   5.282 1.79e-07 ***
#> topo_elevation             -6.039e-05  8.326e-06  -7.253 1.27e-12 ***
#> topo_slope                  1.710e-03  1.607e-03   1.064 0.287743    
#> humidity_range             -7.068e-03  7.024e-04 -10.064  < 2e-16 ***
#> topo_diversity              4.630e-03  1.035e-03   4.473 9.22e-06 ***
#> soil_clay                   9.984e-04  5.495e-04   1.817 0.069708 .  
#> cloud_cover_range           1.352e-03  3.792e-04   3.564 0.000394 ***
#> soil_silt                  -1.026e-03  4.380e-04  -2.342 0.019525 *  
#> rainfall_min                3.945e-04  9.916e-05   3.979 7.78e-05 ***
#> growing_season_temperature -2.520e-03  1.016e-03  -2.480 0.013408 *  
#> swi_max                     4.917e-03  2.713e-04  18.123  < 2e-16 ***
#> soil_nitrogen               1.644e-03  4.077e-03   0.403 0.686886    
#> evapotranspiration_range   -1.344e-03  1.396e-04  -9.625  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.09681 on 597 degrees of freedom
#> Multiple R-squared:  0.7932, Adjusted R-squared:  0.789 
#> F-statistic: 190.8 on 12 and 597 DF,  p-value: < 2.2e-16
#> 

#classification formula (character response)
y <- model_formula(
  df = vi_smol,
  response = "vi_categorical",
  predictors = x
)

y
#> vi_categorical ~ topo_elevation + topo_slope + humidity_range + 
#>     topo_diversity + soil_clay + cloud_cover_range + soil_silt + 
#>     rainfall_min + growing_season_temperature + swi_max + soil_nitrogen + 
#>     evapotranspiration_range
#> <environment: 0x55fbc3dbc110>


#polynomial formula (3rd degree)
y <- model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = x,
  term_f = "poly",
  term_args = "degree = 3, raw = TRUE"
)

y
#> vi_numeric ~ poly(topo_elevation, degree = 3, raw = TRUE) + poly(topo_slope, 
#>     degree = 3, raw = TRUE) + poly(humidity_range, degree = 3, 
#>     raw = TRUE) + poly(topo_diversity, degree = 3, raw = TRUE) + 
#>     poly(soil_clay, degree = 3, raw = TRUE) + poly(cloud_cover_range, 
#>     degree = 3, raw = TRUE) + poly(soil_silt, degree = 3, raw = TRUE) + 
#>     poly(rainfall_min, degree = 3, raw = TRUE) + poly(growing_season_temperature, 
#>     degree = 3, raw = TRUE) + poly(swi_max, degree = 3, raw = TRUE) + 
#>     poly(soil_nitrogen, degree = 3, raw = TRUE) + poly(evapotranspiration_range, 
#>     degree = 3, raw = TRUE)
#> <environment: 0x55fbc3dbc110>

#gam formula
y <- model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = x,
  term_f = "s"
)

y
#> vi_numeric ~ s(topo_elevation) + s(topo_slope) + s(humidity_range) + 
#>     s(topo_diversity) + s(soil_clay) + s(cloud_cover_range) + 
#>     s(soil_silt) + s(rainfall_min) + s(growing_season_temperature) + 
#>     s(swi_max) + s(soil_nitrogen) + s(evapotranspiration_range)
#> <environment: 0x55fbc3dbc110>

#random effect
y <- model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = x,
  random_effects = "country_name" #from vi_smol$country_name
)

y
#> vi_numeric ~ topo_elevation + topo_slope + humidity_range + topo_diversity + 
#>     soil_clay + cloud_cover_range + soil_silt + rainfall_min + 
#>     growing_season_temperature + swi_max + soil_nitrogen + evapotranspiration_range + 
#>     (1 | country_name)
#> <environment: 0x55fbc3dbc110>
```
