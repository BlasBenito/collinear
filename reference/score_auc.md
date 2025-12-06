# Compute area under the ROC curve between binomial observations and probabilistic predictions

Internal function to compute the AUC of binomial models within
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md).
Used within
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md),
and
[`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md).
This function is build for speed and it does not check the inputs.

## Usage

``` r
score_auc(o = NULL, p = NULL, ...)
```

## Arguments

- o:

  (required, numeric vector) Binomial observations (values 0 and 1).
  Default: NULL

- p:

  (required, numeric vector) Prediction of binomial model in the range
  0-1. Default: NULL

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

numeric: Area Under the ROC Curve

## See also

Other modelling_tools:
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md),
[`model_formula()`](https://blasbenito.github.io/collinear/reference/model_formula.md),
[`score_cramer()`](https://blasbenito.github.io/collinear/reference/score_cramer.md),
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)

## Examples

``` r
  score_auc(
    o = c(1, 1, 1, 1, 0, 0, 0),
    p = c(1, 0.8, 0.7, 0.6, 0.5, 0.6, 0.7)
  )
#> [1] 0.8333333
```
