# Compute Cramer's V between categorical observations and predictions

Internal function to compute the Cramer's V of categorical observations
versus categorical model predictions. Please read the help file of
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md)
for further details.

## Usage

``` r
score_cramer(o = NULL, p = NULL, ...)
```

## Arguments

- o:

  (required; character vector) categorical observations. Default: NULL

- p:

  (required; character vector) categorical predictions. Default: NULL

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

numeric: Cramer's V

## See also

Other modelling_tools:
[`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md),
[`model_formula()`](https://blasbenito.github.io/collinear/reference/model_formula.md),
[`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md),
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)

## Examples

``` r
score_cramer(
 o = c("a", "a", "b", "c", "c"),
 p = c("a", "b", "b", "c", "c")
 )
#> [1] 0.5
```
