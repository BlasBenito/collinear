# Large example dataframe

The response variable is a Vegetation Index encoded in different ways to
help highlight the package capabilities:

- `vi_numeric`: continuous vegetation index values in the range 0-1.

- `vi_counts`: simulated integer counts created by multiplying
  `vi_numeric` by 1000 and coercing the result to integer.

- `vi_binomial`: simulated integer binomial variable created by
  transforming `vi_numeric` to zeros and ones.

- `vi_categorical`: character variable with the categories "very_low",
  "low", "medium", "high", and "very_high", with thresholds located at
  the quantiles of `vi_numeric`.

- `vi_factor`: `vi_categorical` converted to factor.

The names of all predictors (continuous, integer, character, and
factors) are in
[vi_predictors](https://blasbenito.github.io/collinear/reference/vi_predictors.md).

## Usage

``` r
data(vi)
```

## Format

dataframe with 30.000 rows and 68 columns.

## See also

[vi_predictors](https://blasbenito.github.io/collinear/reference/vi_predictors.md)

Other example_data:
[`toy`](https://blasbenito.github.io/collinear/reference/toy.md),
[`vi_predictors`](https://blasbenito.github.io/collinear/reference/vi_predictors.md),
[`vi_predictors_categorical`](https://blasbenito.github.io/collinear/reference/vi_predictors_categorical.md),
[`vi_predictors_numeric`](https://blasbenito.github.io/collinear/reference/vi_predictors_numeric.md),
[`vi_responses`](https://blasbenito.github.io/collinear/reference/vi_responses.md),
[`vi_smol`](https://blasbenito.github.io/collinear/reference/vi_smol.md)
