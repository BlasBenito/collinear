# Area Under the Curve of Binomial Random Forest predictions vs. observations

Fits a univariate random forest model `y ~ x` with the binomial (values
0 and 1) response `y` and the numeric, character or factor predictor `x`
using
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
and returns the area under the ROC curve between the observed responses
and the model predictions (see
[`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md)).

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
f_binomial_rf(df, ...)
```

## Arguments

- df:

  (required, dataframe) with columns:

  - "x": (numeric, character, factor) predictor.

  - "y" (integer) binomial response with unique values 0 and 1.

- ...:

  (optional) Accepts the arguments `cv_training_fraction` (numeric
  between 0 and 1) and `cv_iterations` (integer between 1 and Inf) for
  cross validation.

## Value

numeric or numeric vector: AUC

## See also

Other preference_order_functions:
[`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md),
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md),
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
  y = vi_smol[["vi_binomial"]],
  x = vi_smol[["swi_max"]]
)

#no cross-validation
f_binomial_rf(df = df)
#> [1] 0.8995467

#cross-validation
f_binomial_rf(
  df = df,
  cv_training_fraction = 0.5,
  cv_iterations = 10
  )
#>  [1] 0.7632825 0.7761184 0.7781599 0.7363415 0.7222196 0.7676069 0.7459511
#>  [8] 0.7651879 0.7656979 0.7516886

#categorical predictor
df <- data.frame(
  y = vi_smol[["vi_binomial"]],
  x = vi_smol[["koppen_zone"]]
)

f_binomial_rf(df = df)
#> [1] 0.9328657
```
