# Compute variance inflation factors from a correlation matrix

Computes the Variance Inflation Factors from a correlation matrix in two
steps:

- Applies [`base::solve()`](https://rdrr.io/r/base/solve.html) to
  transform the correlation matrix into a precision matrix, which is the
  inverse of the covariance matrix between all variables in
  `predictors`.

- Applies [`base::diag()`](https://rdrr.io/r/base/diag.html) to extract
  the diagonal of the precision matrix, which contains the variance of
  the regression of each predictor against all other predictors, also
  known as Variance Inflation Factor

## Usage

``` r
vif(m = NULL, quiet = FALSE, ...)
```

## Arguments

- m:

  (required, matrix) Correlation matrix generated via
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) or
  [`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md).
  Must have named dimensions. Default: NULL

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

named numeric vector

## Variance Inflation Factors

VIF for predictor \\a\\ is computed as \\1/(1-R^2)\\, where \\R^2\\ is
the multiple R-squared from regressing \\a\\ on the other predictors.
Recommended maximums commonly used are 2.5, 5, and 10.

## References

- David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression
  Diagnostics: Identifying Influential Data and Sources of Collinearity.
  John Wiley & Sons. DOI: 10.1002/0471725153.

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md),
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Examples

``` r
data(vi_smol, vi_predictors_numeric)

m <- cor_matrix(
  df = vi_smol,
  predictors = vi_predictors_numeric[1:5]
)

vif(m)
#>       swi_mean        swi_max     topo_slope topo_diversity topo_elevation 
#>         5.4512         5.0707         1.6719         1.5817         1.4120 
```
