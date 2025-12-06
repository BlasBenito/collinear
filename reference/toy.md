# Toy dataframe with varying levels of multicollinearity

Dataframe with known relationship between responses and predictors
useful to illustrate multicollinearity concepts.

## Usage

``` r
data(toy)
```

## Format

dataframe with 2000 rows and 5 columns.

## Details

Columns:

- `y`: response variable generated from `a * 0.75 + b * 0.25 + noise`.

- `a`: most important predictor of `y`, uncorrelated with `b`.

- `b`: second most important predictor of `y`, uncorrelated with `a`.

- `c`: generated from `a + noise`.

- `d`: generated from `(a + b)/2 + noise`.

These are variance inflation factors of the predictors in `toy`.
variable vif b 4.062 d 6.804 c 13.263 a 16.161

## See also

Other example_data:
[`vi`](https://blasbenito.github.io/collinear/reference/vi.md),
[`vi_predictors`](https://blasbenito.github.io/collinear/reference/vi_predictors.md),
[`vi_predictors_categorical`](https://blasbenito.github.io/collinear/reference/vi_predictors_categorical.md),
[`vi_predictors_numeric`](https://blasbenito.github.io/collinear/reference/vi_predictors_numeric.md),
[`vi_responses`](https://blasbenito.github.io/collinear/reference/vi_responses.md),
[`vi_smol`](https://blasbenito.github.io/collinear/reference/vi_smol.md)
