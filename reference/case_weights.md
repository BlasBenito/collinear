# Generate sample weights for imbalanced responses

Computes case weights adding to one for response variables of these
types:

- numeric binomial (1 and 0).

- logical (TRUE and FALSE): converted to numeric internally.

- categorical (character or factor)

Values NA, Inf, -Inf, and NaN are invalid for numeric and logical
variables and will result in errors. For categorical variables, these
are converted to their respective categories ("NA", "Inf", "-Inf", and
"NaN") with their assigned case weights.

All returned weights sum to one.

## Usage

``` r
case_weights(x = NULL, ...)
```

## Arguments

- x:

  (required, integer, character, or factor vector) Values of a binomial,
  categorical, or factor variable. Default: NULL

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

numeric vector: case weights

## See also

Other modelling_tools:
[`model_formula()`](https://blasbenito.github.io/collinear/reference/model_formula.md),
[`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md),
[`score_cramer()`](https://blasbenito.github.io/collinear/reference/score_cramer.md),
[`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)

## Examples

``` r
 #numeric vector
 y <- case_weights(
   x = c(0, 0, 1, 1)
   )

 #logical vector
 y <- case_weights(
   x = c(TRUE, TRUE, FALSE, FALSE)
   )

 #character vector
 y <- case_weights(
   x = c("a", "a", "b", "c")
   )
```
