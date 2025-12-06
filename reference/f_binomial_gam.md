# Area under the curve of binomial GAM predictions vs. observations

Fits a Quasibinomial GAM model `y ~ s(x)` (`y ~ x` if `x` is
non-numeric) with the binomial response `y` (values 0 and 1) and the
numeric, character or factor predictor `x` using
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and returns the
area under the ROC curve between the observed responses and the model
predictions (see
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
f_binomial_gam(df, ...)
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
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md),
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
f_binomial_gam(df = df)
#> [1] 0.7958704

#cross-validation
f_binomial_gam(
  df = df,
  cv_training_fraction = 0.5,
  cv_iterations = 10
  )
#>  [1] 0.7405760 0.7577502 0.7527259 0.7765308 0.7751319 0.7897904 0.7938672
#>  [8] 0.7890389 0.7977861 0.7474687

#categorical predictor
df <- data.frame(
  y = vi_smol[["vi_binomial"]],
  x = vi_smol[["koppen_zone"]]
)

f_binomial_gam(df = df)
#> [1] 0.9334159
```
