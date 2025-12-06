# Summarize all results of `collinear()`

Summarize all results of
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)

## Usage

``` r
# S3 method for class 'collinear_output'
summary(object = NULL, ...)
```

## Arguments

- object:

  (required, list of class `collinear_output`) Object to summarize.
  Default: NULL

- ...:

  Ignored, kept for consistency with generic.

## Value

list: If `object` was created with `responses = NULL`, a sublist named
"result" containing a vector with the selected predictors. Otherwise, a
list named after each response containing the corresponding variable
selection.

## See also

Other S3_methods:
[`print.collinear_output()`](https://blasbenito.github.io/collinear/reference/print.collinear_output.md),
[`print.collinear_selection()`](https://blasbenito.github.io/collinear/reference/print.collinear_selection.md),
[`summary.collinear_selection()`](https://blasbenito.github.io/collinear/reference/summary.collinear_selection.md)
