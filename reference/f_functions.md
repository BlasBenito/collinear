# List predictor scoring functions

List predictor scoring functions

## Usage

``` r
f_functions()
```

## Value

dataframe

## See also

Other preference_order_tools:
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md),
[`f_auto_rules()`](https://blasbenito.github.io/collinear/reference/f_auto_rules.md)

## Examples

``` r
f_functions()
#>                name response_type predictors_types
#> 1     f_numeric_glm       numeric            mixed
#> 2     f_numeric_gam       numeric          numeric
#> 3      f_numeric_rf       numeric            mixed
#> 4        f_count_rf       integer            mixed
#> 5       f_count_glm       integer            mixed
#> 6       f_count_gam       integer            mixed
#> 7    f_binomial_glm      binomial            mixed
#> 8    f_binomial_gam      binomial          numeric
#> 9     f_binomial_rf      binomial            mixed
#> 10 f_categorical_rf   categorical            mixed
#>                                                                                           expression
#> 1                                            stats::glm(y ~ x, family = gaussian(link = 'identity'))
#> 2                                          mgcv::gam(y ~ s(x), family = gaussian(link = 'identity'))
#> 3                                                                              ranger::ranger(y ~ x)
#> 4                                                                              ranger::ranger(y ~ x)
#> 5                                                  stats::glm(y ~ x, family = poisson(link = 'log'))
#> 6                                                mgcv::gam(y ~ s(x), family = poisson(link = 'log'))
#> 7               stats::glm(y ~ x, family = quasibinomial(link = 'logit'), weights = case_weights(y))
#> 8  mgcv::gam(y ~ s(x), family = quasibinomial(link = 'logit'), weights = collinear::case_weights(y))
#> 9                                   ranger::ranger(y ~ x, case.weights = collinear::case_weights(y))
#> 10                                  ranger::ranger(y ~ x, case.weights = collinear::case_weights(y))
#>        metric
#> 1   R-squared
#> 2   R-squared
#> 3   R-squared
#> 4   R-squared
#> 5   R-squared
#> 6   R-squared
#> 7         AUC
#> 8         AUC
#> 9         AUC
#> 10 Cramer's V
```
