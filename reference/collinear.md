# Smart multicollinearity management

Automates multicollinearity management in datasets with mixed variable
types (numeric, categorical, and logical) through an integrated system
of five key components:

- Target Encoding Integration (opt-in):

  When `responses` is numeric, categorical predictors can be converted
  to numeric using response values as reference. This enables VIF and
  correlation analysis across mixed types. See
  [`target_encoding_lab`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md).

- Intelligent Predictor Ranking (active by default):

  Three prioritization strategies ensure the most relevant predictors
  are retained during filtering:

  - **User-defined ranking** (argument `preference_order`): Accepts a
    character vector of predictor names or a dataframe from
    [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md).
    Lower-ranked collinear predictors are removed.

  - **Response-based ranking** (`f`): Uses
    [`f_auto`](https://blasbenito.github.io/collinear/reference/f_auto.md),
    [`f_numeric_glm`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
    or
    [`f_binomial_rf`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md)
    to rank predictors by association with the response. Supports
    cross-validation via
    [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md).

  - **Multicollinearity-based ranking** (default): When both
    `preference_order` and `f` are `NULL`, predictors are ranked from
    lower to higher multicollinearity.

- Unified Correlation Framework (active by default):

  Computes pairwise correlations between variable types using Pearson
  (numeric–numeric), target encoding (numeric–categorical), and Cramer's
  V (categorical–categorical). See
  [`cor_df`](https://blasbenito.github.io/collinear/reference/cor_df.md),
  [`cor_matrix`](https://blasbenito.github.io/collinear/reference/cor_matrix.md),
  and
  [`cor_cramer`](https://blasbenito.github.io/collinear/reference/cor_cramer.md).

- Adaptive Filtering Thresholds (active by default):

  When `max_cor` and `max_vif` are both `NULL`, thresholds are
  determined from the median correlation structure of the predictors.

- Dual Filtering Strategy (active by default):

  Combines two complementary methods while respecting predictor
  rankings:

  - **Pairwise Correlation Filtering**: Removes predictors with Pearson
    correlation or Cramer's V above `max_cor`. See
    [`cor_select`](https://blasbenito.github.io/collinear/reference/cor_select.md).

  - **VIF-based Filtering**: Removes numeric predictors with VIF above
    `max_vif`. See
    [`vif_select`](https://blasbenito.github.io/collinear/reference/vif_select.md),
    [`vif_df`](https://blasbenito.github.io/collinear/reference/vif_df.md),
    and
    [`vif`](https://blasbenito.github.io/collinear/reference/vif.md).

This function accepts parallelization via
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
and progress bars via
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html).
Parallelization benefits
[`target_encoding_lab`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md),
[`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md),
and
[`cor_select`](https://blasbenito.github.io/collinear/reference/cor_select.md).

## Usage

``` r
collinear(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  encoding_method = NULL,
  preference_order = NULL,
  f = f_auto,
  max_cor = NULL,
  max_vif = NULL,
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

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- encoding_method:

  (optional; character or NULL) One of "loo", "mean", or "rank". If
  NULL, target encoding is disabled. Default: NULL.

- preference_order:

  (optional; character vector, dataframe from
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md),
  or NULL) Prioritizes predictors to preserve.

- f:

  (optional; unquoted function name or NULL) Function to rank predictors
  by relationship with `responses`. See
  [`f_functions`](https://blasbenito.github.io/collinear/reference/f_functions.md).
  Default: `f_auto`.

- max_cor:

  (optional; numeric or NULL) Maximum allowed pairwise correlation
  (0.01–0.99). Recommended between 0.5 and 0.9. If NULL and `max_vif` is
  NULL, it is selected automatically. Default: NULL.

- max_vif:

  (optional; numeric or NULL) Maximum allowed VIF. Recommended between
  2.5 and 10. If NULL and `max_cor` is NULL, configured automatically.
  Default: NULL.

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

A list of class `collinear_output` with sublists of class
`collinear_selection`. If `responses = NULL` a single sublist named
"result" is returned; otherwise a sublist per response is returned.

## Adaptive Multicollinearity Thresholds

When both `max_cor` and `max_vif` are `NULL`, the function determines
thresholds as follows:

1.  Compute the 75th percentile of pairwise correlations via
    [`cor_stats`](https://blasbenito.github.io/collinear/reference/cor_stats.md).

2.  Map that value through a sigmoid between 0.545 (VIF~2.5) and 0.785
    (VIF~7.5), centered at 0.665, to get `max_cor`.

3.  Compute `max_vif` from `max_cor` using
    [`gam_cor_to_vif`](https://blasbenito.github.io/collinear/reference/gam_cor_to_vif.md).

## Variance Inflation Factors

VIF for predictor \\a\\ is computed as \\1/(1-R^2)\\, where \\R^2\\ is
the multiple R-squared from regressing \\a\\ on the other predictors.
Recommended maximums commonly used are 2.5, 5, and 10.

## VIF-based Filtering

[`vif_select`](https://blasbenito.github.io/collinear/reference/vif_select.md)
ranks numeric predictors (user `preference_order` if provided, otherwise
from lower to higher VIF) and sequentially adds predictors whose VIF
against the current selection is below `max_vif`.

## Pairwise Correlation Filtering

[`cor_select`](https://blasbenito.github.io/collinear/reference/cor_select.md)
computes the global correlation matrix, orders predictors by
`preference_order` or by lower-to-higher summed correlations, and
sequentially selects predictors with pairwise correlations below
`max_cor`.

## References

- David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression
  Diagnostics: Identifying Influential Data and Sources of Collinearity.
  John Wiley & Sons. DOI: 10.1002/0471725153.

- Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality
  Categorical Attributes in Classification and Prediction Problems.
  SIGKDD Explor. Newsl. 3, 1, 27-32. DOI: 10.1145/507533.507538

## See also

Other multicollinearity_filtering:
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
[`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md),
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)

## Examples

``` r
data(vi_smol, vi_predictors_numeric)
x <- collinear(df = vi_smol[, vi_predictors_numeric])
#> 
#> collinear::collinear(): setting 'max_cor' to 0.618.
#> 
#> collinear::collinear(): setting 'max_vif' to 5.0318.
#> 
#> collinear::collinear()
#> └── collinear::validate_arg_preference_order()
#>     └── collinear::preference_order(): ranking 47 'predictors' from lower to higher multicollinearity.
#> 
#> collinear::collinear(): selected predictors: 
#>  - topo_elevation
#>  - topo_slope
#>  - humidity_range
#>  - topo_diversity
#>  - soil_clay
#>  - cloud_cover_range
#>  - soil_silt
#>  - rainfall_min
#>  - growing_season_temperature
#>  - swi_max
#>  - soil_nitrogen
#>  - temperature_seasonality
#>  - rainfall_max
```
