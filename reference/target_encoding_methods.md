# Encode categories as response means

Encode categories as response means

## Usage

``` r
target_encoding_loo(
  df = NULL,
  response = NULL,
  predictor = NULL,
  encoded_name = NULL,
  smoothing = NULL,
  ...
)

target_encoding_mean(
  df = NULL,
  response = NULL,
  predictor = NULL,
  encoded_name = NULL,
  smoothing = 0,
  ...
)

target_encoding_rank(
  df = NULL,
  response = NULL,
  predictor = NULL,
  encoded_name = NULL,
  smoothing = NULL,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- response:

  (optional, character string) Name of a numeric response variable in
  `df`. Default: NULL.

- predictor:

  (required; string) Name of the categorical predictor to encode.
  Default: NULL

- encoded_name:

  (optional, string) Name of the encoded predictor. Default: NULL

- smoothing:

  (optional; integer) Groups smaller than this number have their means
  pulled towards the mean of the response across all cases. Ignored by
  `target_encoding_rank()` and `target_encoding_loo()`. Default: 0

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

dataframe

## See also

Other target_encoding:
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)

Other target_encoding:
[`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)

## Examples

``` r
#  loading example data
data(vi_smol)

#mean encoding
#-------------

df <- target_encoding_mean(
  df = vi_smol,
  response = "vi_numeric",
  predictor = "soil_type", #categorical
  encoded_name = "soil_type_encoded"
)

if(interactive()){

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

}


#rank encoding
#----------

df <- target_encoding_rank(
  df = vi_smol,
  response = "vi_numeric",
  predictor = "soil_type",
  encoded_name = "soil_type_encoded"
)

if(interactive()){

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

}


#leave-one-out
#-------------

df <- target_encoding_loo(
  df = vi_smol,
  response = "vi_numeric",
  predictor = "soil_type",
  encoded_name = "soil_type_encoded"
)

if(interactive()){

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

}
```
