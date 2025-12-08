# Intelligent Predictor Ranking

## Summary

The package `collinear` implements an automated multicollinearity
filtering method devised to preserve as many *relevant* predictors as
possible. This principle helps balance multicollinearity reduction with
predictive power retention.

This feature is implemented in
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
and
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
via the argument `preference_order`. This argument allows representing
predictor relevance in three ways:

- **Expert Mode**: Vector of predictor names ordered from left to right
  according to the user’s preference. This option helps
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
  get domain expertise into account, and lets the user focus on specific
  predictors.

- **Intelligent Predictor Ranking**: This functionality, implemented in
  [`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md),
  prioritizes predictors by their univariate association with the
  response to ensure that the most relevant ones are retained during
  multicollinearity filtering. This option maximizes the predictive
  power of the filtered predictors.

- **Naive Option**: If none of the options above are used,
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
  ranks predictors from lower to higher collinearity with all other
  predictors. This option preserves the less redundant predictors, but
  it might not lead to robust models.

These options are explained in detail in the following sections.

## Setup

This article requires the following setup.

``` r
library(collinear)
library(future)
library(DT)

#parallelization setup
#only useful for categorical predictors
future::plan(
  future::multisession,
  workers = future::availableCores() - 1
)

#progress bar (does not work in Rmarkdown)
#progressr::handlers(global = TRUE)

#example data
data(vi_smol, vi_predictors_numeric)
```

## Expert Mode

Let’s consider a little hypothetical.

The user has dataframe `x` with three variables `a`, `b` and `c`, and
domain knowledge indicating that `a` and `b` are key and should be
preserved when possible. Then, the user calls
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
as follows:

``` r
y <- collinear::collinear(
  df = x,
  predictors = c("a", "b", "c"),
  preference_order = c("a", "b"),
  max_cor = 0.5
)
```

Notice that the argument `responses` is missing: this option ignores it,
making a response variable entirely optional.

What happens from here?:

- `a`: Selected.
- `b`: Selected if its correlation with `"a"` is \<= 0.5, and filtered
  away otherwise.
- `c`: Selected if its maximum correlation with `a` and `b` is \<= 0.5,
  and filtered away otherwise.

In summary, the first predictor in `preference_order` is always
selected, and the other ones are selected or rejected conditionally on
their collinearity with the already selected ones.

In case you wonder: `predictors` not in `preference_order` are ranked
from lower to higher collinearity among themselves, and added in such
order to the preference vector.

Let’s take a look at a more tangible case now. The code below calls
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
on the dataset
[`vi_smol`](https://blasbenito.github.io/collinear/articles/reference/vi_smol.md),
which contains a numeric response
[`vi_numeric`](https://blasbenito.github.io/collinear/articles/reference/vi_numeric.md)
(values of a vegetation index) and a bunch of numeric predictors named
in the vector
[`vi_predictors_numeric`](https://blasbenito.github.io/collinear/articles/reference/vi_predictors_numeric.md).

Let’s say we’d like to focus our analysis in the limiting role of the
soil water content (variables `swi_xxx`, from soil water index) in
controlling `vi_numeric`. In such case, we can call
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
as follows:

``` r
y <- collinear::collinear(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = c(
    "swi_min",
    "swi_max",
    "swi_mean",
    "swi_range"
  ),
  max_cor = 0.5,
  max_vif = 2.5,
  quiet = TRUE
)

y$vi_numeric$selection
#> [1] "swi_min"                    "swi_range"                 
#> [3] "topo_elevation"             "topo_slope"                
#> [5] "humidity_range"             "soil_clay"                 
#> [7] "soil_silt"                  "rainfall_min"              
#> [9] "growing_season_temperature"
#> attr(,"validated")
#> [1] TRUE
```

Notice how `swi_min` and `swi_range` are selected, but `swi_max` and
`swi_mean` are removed because they are collinear with `swi_min`. As
said above, all predictors not in the argument `preference_order` were
ranked from lower to higher mutual collinearity.

Now we can quickly fit a quick exploratory model, and save the R-squared
for later.

``` r
m1 <- stats::lm(
  formula = y$vi_numeric$formulas$linear,
  data = y$vi_numeric$df
) |> 
  summary()

m1
#> 
#> Call:
#> stats::lm(formula = y$vi_numeric$formulas$linear, data = y$vi_numeric$df)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.42802 -0.06894 -0.00307  0.07331  0.32260 
#> 
#> Coefficients:
#>                              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                 1.261e-02  3.004e-02   0.420  0.67490    
#> swi_min                     5.591e-03  4.769e-04  11.723  < 2e-16 ***
#> swi_range                   7.739e-03  3.072e-04  25.191  < 2e-16 ***
#> topo_elevation             -2.709e-05  8.601e-06  -3.149  0.00172 ** 
#> topo_slope                  7.592e-03  1.551e-03   4.895 1.26e-06 ***
#> humidity_range             -8.134e-03  7.503e-04 -10.840  < 2e-16 ***
#> soil_clay                   1.120e-03  6.266e-04   1.788  0.07433 .  
#> soil_silt                  -1.257e-03  4.987e-04  -2.520  0.01200 *  
#> rainfall_min                7.244e-04  1.008e-04   7.190 1.94e-12 ***
#> growing_season_temperature  3.418e-03  8.626e-04   3.963 8.29e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.1079 on 600 degrees of freedom
#> Multiple R-squared:  0.7418, Adjusted R-squared:  0.7379 
#> F-statistic: 191.5 on 9 and 600 DF,  p-value: < 2.2e-16
```

## Intelligent Predictor Ranking

Let’s go back to our little hypothetical with the dataframe `x`, and the
three variables `a`, `b` and `c`. But this time we also have a response
`y`, and a user with not as much domain knowledge as they’d like (it
happens, I’ve seen it).

In this case, `collinear` first fits the univariate models `y ~ a`,
`y ~ b`, and `y ~ c`, computes the R-squared between observations and
model predictions, and ranks the predictors from best to worse according
to this metric.

This functionality is implemented in the function
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md),
which can take advantage of a parallelization backend to speed-up
operations.

Let’s take a look at how it works, step by step. Let me start with the
simplest approach.

``` r
x <- collinear::preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric,
  quiet = TRUE
)
```

The function returns a dataframe with the predictors ordered from better
to worse modelling performance against the response. The column `f`
indicates the name of the function used to fit the univariate models,
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md)
in this case. This function has been selected automatically because the
argument `f` of
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
is set to
[`f_auto`](https://blasbenito.github.io/collinear/articles/reference/f_auto.md)
by default (`f` functions must not have parenthesis when calling them
via the `f` argument). This function looks at the types of the responses
and predictors, and select one of the functions in returned by
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)
to perform the operation.

Let’s talk more about that later, but for now, we can plug the
preference order dataframe directly into
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).

``` r
y <- collinear::collinear(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = x,
  max_cor = 0.5,
  max_vif = 2.5,
  quiet = TRUE
)

y$vi_numeric$selection
#> [1] "growing_season_length" "swi_min"               "rainfall_min"         
#> [4] "solar_rad_range"       "cloud_cover_range"     "soil_clay"            
#> [7] "topo_diversity"        "soil_silt"            
#> attr(,"validated")
#> [1] TRUE
```

Again, we can use the
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
output to fit a little model.

``` r
m2 <- stats::lm(
  formula = y$vi_numeric$formulas$linear,
  data = y$vi_numeric$df
) |> 
  summary()

m2
#> 
#> Call:
#> stats::lm(formula = y$vi_numeric$formulas$linear, data = y$vi_numeric$df)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.39369 -0.04100 -0.00053  0.04417  0.32204 
#> 
#> Coefficients:
#>                         Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)            1.507e-01  2.396e-02   6.291 6.08e-10 ***
#> growing_season_length  1.271e-03  4.163e-05  30.523  < 2e-16 ***
#> swi_min                3.844e-03  3.916e-04   9.815  < 2e-16 ***
#> rainfall_min           9.932e-06  8.123e-05   0.122   0.9027    
#> solar_rad_range       -3.609e-03  6.969e-04  -5.179 3.06e-07 ***
#> cloud_cover_range      5.419e-04  2.880e-04   1.882   0.0603 .  
#> soil_clay             -2.776e-04  4.865e-04  -0.571   0.5685    
#> topo_diversity         7.559e-04  7.380e-04   1.024   0.3062    
#> soil_silt             -1.749e-03  3.864e-04  -4.527 7.21e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.08315 on 601 degrees of freedom
#> Multiple R-squared:  0.8464, Adjusted R-squared:  0.8443 
#> F-statistic: 413.9 on 8 and 601 DF,  p-value: < 2.2e-16
```

If we compare the R-squared of the two models we have created so far, we
can see that
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
helps balance aggressive multicollinearity filtering and robust
modelling outcomes.

``` r
m1$r.squared
#> [1] 0.7417704
m2$r.squared
#> [1] 0.8463813
```

Let’s go back to what
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md)
does for a moment. This function looks at the input data to assess the
type of the response and the predictors, and then looks at the dataframe
below to choose a function.

``` r
collinear::f_auto_rules()
```

You can see it in action across different settings below.

``` r
collinear::f_auto(
  df = vi_smol,
  response = "vi_categorical",
  predictors = vi_predictors_categorical,
  quiet = TRUE
)
#> [1] "f_categorical_rf"
```

``` r
collinear::f_auto(
  df = vi_smol,
  response = "vi_binomial", #ones and zeros
  predictors = vi_predictors_numeric,
  quiet = TRUE
)
#> [1] "f_binomial_glm"
```

``` r
collinear::f_auto(
  df = vi_smol,
  response = "vi_counts", #integer counts
  predictors = vi_predictors_numeric,
  quiet = TRUE
)
#> [1] "f_count_glm"
```

``` r
collinear::f_auto(
  df = vi_smol,
  response = "vi_counts",
  predictors = vi_predictors, #numeric and categorical
  quiet = TRUE
)
#> [1] "f_count_rf"
```

All `f_...()` functions available for usage in
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
are listed in the dataframe returned by
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md).

``` r
collinear::f_functions()
```

Once you know your way around these functions, you can choose the one
you prefer for your case. For example, below we replace
[`f_auto`](https://blasbenito.github.io/collinear/articles/reference/f_auto.md)
with
[`f_numeric_gam`](https://blasbenito.github.io/collinear/articles/reference/f_numeric_gam)
to fit univariate GAM models.

``` r
x <- collinear::preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric,
  f = f_numeric_gam,
  quiet = TRUE
)
```

A gentle reminder to finish this section:
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
runs
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
internally when `preference_order = NULL` and the argument `f` receives
a valid function. And like
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md),
it can use cross-validation to assess the association between response
and predictor in a more robust manner.

``` r
y <- collinear::collinear(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = NULL,
  f = f_numeric_glm,
  quiet = FALSE,
  cv_iterations = 100, #number of repetitions
  cv_training_fraction = 0.5 #50% rows of vi_smol
)
#> 
#> collinear::collinear(): setting 'max_cor' to 0.618.
#> 
#> collinear::collinear(): setting 'max_vif' to 5.0318.
#> 
#> collinear::collinear(): selected predictors: 
#>  - growing_season_length
#>  - cloud_cover_min
#>  - temperature_seasonality
#>  - cloud_cover_range
#>  - evapotranspiration_mean
#>  - soil_clay
#>  - topo_diversity
#>  - humidity_range
#>  - topo_elevation
#>  - topo_slope
#>  - soil_silt
```

The output of
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
is returned by
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).

``` r
y$vi_numeric$preference_order
```

## Naive Option

For this final option, our hypothetical user does not care about what I
have written above, and sets `f = NULL` in
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md).

In this scenario,
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
computes the pairwise correlation between all pairs of predictors `a`,
`b`, and `c` with
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
and sums the correlations of each predictor with all others. Finally, it
ranks the predictors from lowest to highest sum of correlations.

This option gives preference to those predictors that contain more
*exclusive* information, but in exchange, might not lead to robust
models.

``` r
x <- collinear::preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric,
  f = NULL
)
#> 
#> collinear::preference_order(): ranking 47 'predictors' from lower to higher multicollinearity.
```

The output shows a column `score` computed as 1 minus the sum of
correlations, as indicated in the column `metric`.

Let’s use this ranking in
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
to then fit a linear model.

``` r
y <- collinear::collinear(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric,
  preference_order = x,
  max_cor = 0.5,
  max_vif = 2.5,
  quiet = TRUE
)

m3 <- stats::lm(
  formula = y$vi_numeric$formulas$linear,
  data = y$vi_numeric$df
) |> 
  summary()
```

And finally, an informal comparison between the three preference order
methods described in this article.

``` r
#expert mode: focused on specific variables (swi_...)
m1$r.squared
#> [1] 0.7417704
```

``` r
#intelligent predictor ranking: optimized for prediction
m2$r.squared
#> [1] 0.8463813
```

``` r
#naive option: minimizes redundancy, not optimized for prediction
m3$r.squared
#> [1] 0.7494892
```

Please, take in mind that these R-squared values are just coarse
indicators of model robustness, and should not be interpreted as proof
that one method is better than any other.
