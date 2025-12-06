# Find valid categorical variables in a dataframe

Identifies valid and invalid character or factor variables. Invalid
categorical predictors are those with a single category, or as many
categories as cases (full-cardinality).

## Usage

``` r
identify_categorical_variables(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- responses:

  (optional; character, character vector, or NULL) Name of one or
  several response variables in `df`. Default: NULL.

- predictors:

  (required, character vector) Names of the predictors to identify.
  Default: NULL

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

list:

- `valid`: character vector with valid categorical predictor names.

- `invalid`: character vector with invalid categorical predictor names
  due to degenerate cardinality (1 or `nrow(df)` categories).

## See also

Other data_types:
[`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md),
[`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md),
[`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md),
[`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md),
[`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol, vi_predictors)

#create an invalid categorical
vi_smol$invalid_categorical <- "a"

x <- identify_categorical_variables(
  df = vi_smol,
  responses = "vi_categorical",
  predictors = vi_predictors
)

x$valid
#>  [1] "vi_categorical"     "koppen_zone"        "koppen_group"      
#>  [4] "koppen_description" "soil_type"          "biogeo_ecoregion"  
#>  [7] "biogeo_biome"       "biogeo_realm"       "country_name"      
#> [10] "continent"          "region"             "subregion"         
x$invalid
#> NULL
```
