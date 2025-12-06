# Multicollinearity filtering by variance inflation factor threshold

Wraps
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
to automatize multicollinearity filtering via variance inflation factors
(VIF) in dataframes with numeric and categorical predictors.

The argument `max_vif` determines the maximum variance inflation factor
allowed in the resulting selection of predictors.

The argument `preference_order` accepts a character vector of predictor
names ranked from first to last index, or a dataframe resulting from
[`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md).
When two predictors in this vector or dataframe are highly collinear,
the one with a lower ranking is removed. This option helps protect
predictors of interest. If not provided, predictors are ranked from
lower to higher multicollinearity.

Please check the sections **Variance Inflation Factors** and **VIF-based
Filtering** at the end of this help file for further details.

## Usage

``` r
vif_select(
  df = NULL,
  response = NULL,
  predictors = NULL,
  preference_order = NULL,
  max_vif = 5,
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

- max_vif:

  (optional, numeric or NULL) Maximum Variance Inflation Factor allowed
  for `predictors` during multicollinearity filtering. Recommended
  values are between 2.5 (strict) and 10 (permissive). Default: 5

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

character vector of selected predictors

## Variance Inflation Factors

VIF for predictor \\a\\ is computed as \\1/(1-R^2)\\, where \\R^2\\ is
the multiple R-squared from regressing \\a\\ on the other predictors.
Recommended maximums commonly used are 2.5, 5, and 10.

## VIF-based Filtering

`vif_select` ranks numeric predictors (user `preference_order` if
provided, otherwise from lower to higher VIF) and sequentially adds
predictors whose VIF against the current selection is below `max_vif`.

## References

- David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression
  Diagnostics: Identifying Influential Data and Sources of Collinearity.
  John Wiley & Sons. DOI: 10.1002/0471725153.

## See also

Other multicollinearity_filtering:
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
[`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md)

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
x <- vif_select(
  df = vi_smol,
  predictors = predictors,
  max_vif = 5
)
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order(): ranking 4 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order()
#>             └── collinear::cor_matrix()
#>                 └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select(): maximum VIF is <= 5, multicollinearity filtering is not required.

x
#> [1] "koppen_zone"           "soil_type"             "topo_elevation"       
#> [4] "soil_temperature_mean"
#> attr(,"validated")
#> [1] TRUE


#with custom preference order
x <- vif_select(
  df = vi_smol,
  predictors = predictors,
  preference_order = c(
    "koppen_zone",
    "soil_type"
  ),
  max_vif = 5
)
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::cor_matrix()
#>         └── collinear::cor_df(): 2 categorical predictors have cardinality > 2 and may bias the multicollinearity analysis. Applying target encoding to convert them to numeric will solve this issue.
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select()
#>     └── collinear::validate_arg_preference_order()
#>         └── collinear::preference_order(): ranking 2 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::vif_select()
#> └── collinear::collinear_select(): maximum VIF is <= 5, multicollinearity filtering is not required.

x
#> [1] "koppen_zone"           "soil_type"             "topo_elevation"       
#> [4] "soil_temperature_mean"
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

## OPTIONAL: disable parallelization
#future::plan(future::sequential)
```
