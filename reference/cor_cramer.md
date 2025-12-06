# Quantify association between categorical variables

Cramer's V extends the chi-squared test to quantify how strongly the
categories of two variables co-occur. The value ranges from 0 to 1,
where 0 indicates no association and 1 indicates perfect association.

This function implements a bias-corrected version of Cramer's V, which
adjusts for sample size and is more accurate for small samples. However,
this bias correction means that even for binary variables, Cramer's V
will no equal the Pearson correlation (the standard, uncorrected
Cramer's V does match Pearson for binary data).

As the number of categories increases, Cramer's V and Pearson
correlation measure increasingly different aspects of association and
should not be directly compared.

If you intend to combine these measures in a multicollinearity analysis,
interpret them with care. It is often preferable to convert non-numeric
variables to numeric form (for example, via target encoding) before
assessing multicollinearity.

## Usage

``` r
cor_cramer(x = NULL, y = NULL, check_input = TRUE, ...)
```

## Arguments

- x:

  (required; vector) Values of a categorical variable (character or
  vector). Converted to character if numeric or logical. Default: NULL

- y:

  (required; vector) Values of a categorical variable (character or
  vector). Converted to character if numeric or logical. Default: NULL

- check_input:

  (required; logical) If FALSE, disables data checking for a slightly
  faster execution. Default: TRUE

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

numeric: Cramer's V

## References

- Cramér, H. (1946). Mathematical Methods of Statistics. Princeton:
  Princeton University Press, page 282 (Chapter 21. The two-dimensional
  case). ISBN 0-691-08004-6

## See also

Other multicollinearity_assessment:
[`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md),
[`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md),
[`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md),
[`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
[`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md),
[`vif()`](https://blasbenito.github.io/collinear/reference/vif.md),
[`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md),
[`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
# perfect one-to-one association
cor_cramer(
  x = c("a", "a", "b", "c"),
  y = c("a", "a", "b", "c")
)
#> [1] 1

# still perfect: labels differ but mapping is unique
cor_cramer(
  x = c("a", "a", "b", "c"),
  y = c("a", "a", "b", "d")
)
#> [1] 1

# high but < 1: mostly aligned, one category of y repeats
cor_cramer(
  x = c("a", "a", "b", "c"),
  y = c("a", "a", "b", "b")
)
#> [1] 0.7071068

# appears similar by position, but no association by distribution
# (x = "a" mixes with y = "a" and "b")
cor_cramer(
  x = c("a", "a", "a", "c"),
  y = c("a", "a", "b", "b")
)
#> [1] 0

# numeric inputs are coerced to character internally
cor_cramer(
  x = c(1, 1, 2, 3),
  y = c(1, 1, 2, 2)
)
#> [1] 0.7071068

# logical inputs are also coerced to character
cor_cramer(
  x = c(TRUE, TRUE, FALSE, FALSE),
  y = c(TRUE, TRUE, FALSE, FALSE)
)
#> [1] 1
```
