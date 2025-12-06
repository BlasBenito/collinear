# Cramer's V of Categorical Random Forest predictions vs. observations

Fits a univariate random forest model `y ~ x` with the character or
factor response `y` and the numeric, character or factor predictor `x`
using
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
and returns the Cramer's V (see
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md))
between the observed responses and the model predictions. Cases are
weighted with
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md)
to prevent issues arising from class imbalance.

Cases are weighted with
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md)
to prevent issues arising from class imbalance.

Supports cross-validation via the arguments arguments
`cv_training_fraction` (numeric between 0 and 1) and `cv_iterations`
(integer between 1 and `n`) introduced via ellipsis (`...`). See
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
for further details.

## Usage

``` r
f_categorical_rf(df, ...)
```

## Arguments

- df:

  (required, dataframe) with columns:

  - `x`: (numeric) numeric, character, or factor predictor.

  - `y` (numeric) character or factor response.

- ...:

  (optional) Accepts the arguments `cv_training_fraction` (numeric
  between 0 and 1) and `cv_iterations` (integer between 1 and Inf) for
  cross validation.

## Value

numeric or numeric vector: Cramer's V

## See also

Other preference_order_functions:
[`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md),
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md),
[`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md),
[`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md),
[`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md),
[`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md),
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
[`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md),
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)

## Examples

``` r
data(vi_smol)

df <- data.frame(
  y = vi_smol[["vi_factor"]],
  x = vi_smol[["soil_type"]]
)

#no cross-validation
f_categorical_rf(df = df)
#> [1] 0.4710376

#cross-validation
f_categorical_rf(
  df = df,
  cv_training_fraction = 0.5,
  cv_iterations = 10
  )
#>  [1] 0.4581166 0.4632736 0.3917833 0.4351281 0.4394164 0.4704325 0.4562418
#>  [8] 0.4325661 0.4756790 0.4481384

#numeric predictor
df <- data.frame(
  y = vi_smol[["vi_categorical"]],
  x = vi_smol[["swi_max"]]
)

f_categorical_rf(df = df)
#> [1] 0.5352031
```
