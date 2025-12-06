# R-squared of Random Forest predictions vs. observations

Fits a univariate random forest model `y ~ x` with the integer response
`y` and the numeric, character or factor predictor `x` using
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
and returns the R-squared of the observations against the predictions
(see
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)).

Supports cross-validation via the arguments arguments
`cv_training_fraction` (numeric between 0 and 1) and `cv_iterations`
(integer between 1 and `n`) introduced via ellipsis (`...`). See
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
for further details.

## Usage

``` r
f_count_rf(df, ...)
```

## Arguments

- df:

  (required, dataframe) with columns:

  - "x": (numeric, character, factor) predictor.

  - "y" (integer) counts response.

- ...:

  (optional) Accepts the arguments `cv_training_fraction` (numeric
  between 0 and 1) and `cv_iterations` (integer between 1 and Inf) for
  cross validation.

## Value

numeric or numeric vector: R-squared

## See also

Other preference_order_functions:
[`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md),
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md),
[`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md),
[`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md),
[`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md),
[`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md),
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
[`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md),
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)

## Examples

``` r
data(vi_smol)

df <- data.frame(
  y = vi_smol[["vi_counts"]],
  x = vi_smol[["swi_max"]]
)

#no cross-validation
f_count_rf(df = df)
#> [1] 0.7174083

#cross-validation
f_count_rf(
  df = df,
  cv_training_fraction = 0.5,
  cv_iterations = 10
  )
#>  [1] 0.6020496 0.4840571 0.5607237 0.6196142 0.5985117 0.5799609 0.5634010
#>  [8] 0.5833623 0.5653907 0.6038178

#categorical predictor
df <- data.frame(
  y = vi_smol[["vi_counts"]],
  x = vi_smol[["koppen_zone"]]
)

f_count_rf(df = df)
#> [1] 0.8173939
```
