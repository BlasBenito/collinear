# R-squared of Random Forest predictions vs. observations

Fits a univariate random forest model `y ~ x` with the numeric response
`y` and the numeric, character or factor predictor `x` using
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
and returns the R-squared between the observed responses and returns the
R-squared of the observations against the predictions (see
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)).

Supports cross-validation via the arguments arguments
`cv_training_fraction` (numeric between 0 and 1) and `cv_iterations`
(integer between 1 and `n`) introduced via ellipsis (`...`). See
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
for further details.

## Usage

``` r
f_numeric_rf(df, ...)
```

## Arguments

- df:

  (required, dataframe) with columns:

  - `x`: (numeric, character, factor) predictor.

  - `y` (numeric) continuous response.

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
[`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md),
[`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md),
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)

## Examples

``` r
data(vi_smol)

df <- data.frame(
  y = vi_smol[["vi_numeric"]],
  x = vi_smol[["swi_max"]]
)

#no cross-validation
f_numeric_rf(df = df)
#> [1] 0.7174085

#cross-validation
f_numeric_rf(
  df = df,
  cv_training_fraction = 0.5,
  cv_iterations = 10
  )
#>  [1] 0.5856970 0.5253044 0.5521651 0.5560779 0.5302282 0.5631309 0.5878490
#>  [8] 0.5775360 0.5966921 0.5266455

#categorical predictor
df <- data.frame(
  y = vi_smol[["vi_numeric"]],
  x = vi_smol[["koppen_zone"]]
)

f_numeric_rf(df = df)
#> [1] 0.8173939
```
