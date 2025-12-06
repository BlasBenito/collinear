# Compute R-squared between numeric observations and predictions

Internal function to compute the R-squared of observations versus
predictions via [`stats::cor()`](https://rdrr.io/r/stats/cor.html). Used
within
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
[`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md),
[`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md),
[`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md),
and
[`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md).

## Usage

``` r
score_r2(o = NULL, p = NULL, ...)
```

## Arguments

- o:

  (required, numeric vector) Observations. Default: NULL

- p:

  (required, numeric vector) Predictions. Default: NULL

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

numeric: Pearson R-squared

## See also

Other modelling_tools:
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md),
[`model_formula()`](https://blasbenito.github.io/collinear/reference/model_formula.md),
[`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md),
[`score_cramer()`](https://blasbenito.github.io/collinear/reference/score_cramer.md)

## Examples

``` r
  score_r2(
    o = c(1, 1, 1, 0.5, 0.5, 0, 0),
    p = c(1, 0.8, 0.7, 0.6, 0.5, 0.1, 0)
  )
#> [1] 0.9077973
```
