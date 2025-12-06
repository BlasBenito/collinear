# Multicollinearity filtering by pairwise correlation threshold

Wraps
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
to automatize multicollinearity filtering via pairwise correlation in
dataframes with numeric and categorical predictors.

The argument `max_cor` determines the maximum variance inflation factor
allowed in the resulting selection of predictors.

The argument `preference_order` accepts a character vector of predictor
names ranked from first to last index, or a dataframe resulting from
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md).
When two predictors in this vector or dataframe are highly collinear,
the one with a lower ranking is removed. This option helps protect
predictors of interest. If not provided, predictors are ranked from
lower to higher multicollinearity.

Please check the section **Pairwise Correlation Filtering** at the end
of this help file for further details.

## Usage

``` r
cor_select(
  df = NULL,
  response = NULL,
  predictors = NULL,
  preference_order = NULL,
  max_cor = 0.7,
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

- response:

  (optional; character or NULL) Name of one response variable in `df`.
  Used to exclude columns when `predictors` is NULL, and to filter
  `preference_order` when it is a dataframe and contains several
  responses. Default: NULL.

- predictors:

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- preference_order:

  (optional; character vector, dataframe from
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md),
  or NULL) Prioritizes predictors to preserve.

- max_cor:

  (optional; numeric or NULL) Maximum correlation allowed between pairs
  of `predictors`. Valid values are between 0.01 and 0.99, and
  recommended values are between 0.5 (strict) and 0.9 (permissive).
  Default: 0.7

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

character vector of selected predictors

## Pairwise Correlation Filtering

`cor_select` computes the global correlation matrix, orders predictors
by `preference_order` or by lower-to-higher summed correlations, and
sequentially selects predictors with pairwise correlations below
`max_cor`.

## See also

Other multicollinearity_filtering:
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md),
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol)

## OPTIONAL: parallelization setup
## irrelevant when all predictors are numeric
## only worth it for large data with many categoricals
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

## OPTIONAL: progress bar
# progressr::handlers(global = TRUE)

#predictors
predictors = c(
  "koppen_zone", #character
  "soil_type", #factor
  "topo_elevation", #numeric
  "soil_temperature_mean" #numeric
)

#predictors ordered from lower to higher multicollinearity
x <- cor_select(
  df = vi_smol,
  predictors = predictors,
  max_cor = 0.7
)
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order(): ranking 4 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order()
#>             └── collinear::cor_matrix()
#>                 └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

x
#> [1] "topo_elevation" "soil_type"      "koppen_zone"   
#> attr(,"validated")
#> [1] TRUE


#with custom preference order
x <- cor_select(
  df = vi_smol,
  predictors = predictors,
  preference_order = c(
    "koppen_zone",
    "soil_type"
  ),
  max_cor = 0.7
)
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order(): ranking 2 'predictors' from lower to higher multicollinearity.

x
#> [1] "koppen_zone"    "soil_type"      "topo_elevation"
#> attr(,"validated")
#> [1] TRUE

#with automated preference order
df_preference <- preference_order(
  df = vi_smol,
  response = "vi_numeric",
  predictors = predictors
)
#> 
#> collinear::preference_order()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::preference_order()
#> └── collinear::f_auto(): selected function 'f_numeric_rf()' to compute preference order.

df_preference
#>     response             predictor            f    metric  score rank
#> 1 vi_numeric           koppen_zone f_numeric_rf R-squared 0.8174    1
#> 2 vi_numeric             soil_type f_numeric_rf R-squared 0.6249    2
#> 3 vi_numeric soil_temperature_mean f_numeric_rf R-squared 0.4201    3
#> 4 vi_numeric        topo_elevation f_numeric_rf R-squared 0.3754    4

x <- cor_select(
  df = vi_smol,
  predictors = predictors,
  preference_order = df_preference,
  max_cor = 0.7
)
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::cor_select()
#> └── collinear::collinear_select()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.

x
#> [1] "koppen_zone"    "soil_type"      "topo_elevation"
#> attr(,"validated")
#> [1] TRUE

#OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
