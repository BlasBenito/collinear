# Rank predictors by importance or multicollinearity

Generates a valid input for the argument `preference_order` of the
functions
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md),
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
and
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).
This argument helps preserve important predictors during
multicollinearity filtering.

The function works in two different ways:

- When `f` is NULL, it ranks the predictors from lower to higher
  multicollinearity, computed as one minus the average Pearson
  correlation between the given predictor against all others. This
  option is useful when the goal is to limit redundancy in a large
  dataset and there is not an specific model to train in mind.

- When `responses` and `f` are not NULL, it ranks the predictors by the
  strength of their association with a response based on the evaluation
  of univariate models. This is the best possible option when the
  end-goal is training a model.

The argument `f` (requires a valid `resopnses` argument) defines how the
strength of association between the response and each predictor is
computed. By default it calls
[`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md),
which uses
[`f_auto_rules()`](https://blasbenito.github.io/collinear/reference/f_auto_rules.md)
to select a suitable function depending on the types of the response and
the predictors. This option is designed to provide sensible,
general-purpose defaults optimized for speed and stability rather than
any specific modeling approach.

For more fine-tuned control, the package offers the following `f`
functions (see
[`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)):

- **Numeric response**:

  - [`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md):
    Pearson's R-squared of response versus the predictions of a Gaussian
    GLM.

  - [`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md):
    GAM model fitted with
    [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

  - [`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md):
    Random Forest model fitted with
    [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md).

- **Integer counts response**:

  - [`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md):
    Pearson's R-squared of a Poisson GLM.

  - [`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md):
    Poisson GAM.

  - [`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md):
    Random Forest model fitted with
    [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md).

- **Binomial response (1 and 0)**:

  - [`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md):
    AUC of Quasibinomial GLM with weighted cases.

  - [`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md):
    AUC of Quasibinomial GAM with weighted cases.

  - [`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md):
    AUC of a Random Forest model with weighted cases.

- **Categorical response**:

  - [`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md):
    Cramer's V of the response against the predictions of a
    classification Random Forest model.

These functions accept a cross-validation setup via the arguments
`cv_iterations` and `cv_training_fraction`.

Additionally, the argument `f` accepts any custom function taking a
dataframe with the columns "x" (predictor) and "y" (response) and
returning a numeric indicator of association.

Accepts a parallelization setup via
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
and a progress bar via
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
(see examples).

Accepts a character vector of response variables as input for the
argument `responses`. When more than one response is provided, the
output is a named list of preference data frames.

## Usage

``` r
preference_order(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  f = f_auto,
  cv_training_fraction = 1,
  cv_iterations = 1,
  seed = 1,
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

- f:

  (optional: function name) Unquoted function name without parenthesis
  (see
  [f_functions](https://blasbenito.github.io/collinear/reference/f_functions.md)).
  By default calls to
  [`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md),
  which selects a suitable function depending on the nature of the
  response and predictors. Set to NULL if `responses = NULL`. If NULL,
  predictors are ranked from lower to higher multicollinearity. Default:
  `f_auto`

- cv_training_fraction:

  (optional, numeric) Value between 0.1 and 1 defining the training
  faction used in cross-validation. If 1 (default), no cross-validation
  is performed, and the resulting metric is computed from all
  observations and predictions. Automatically set to 1 when
  `cv_iterations = 1`. Default: 1

- cv_iterations:

  (optional, integer) Number of cross-validation iterations to perform.
  The recommended range lies between 30 and 100. In general, smaller
  datasets and large values of `cv_training_fraction` require more
  iterations to achieve stability. Automatically set to 1 when
  `cv_training_fraction = 1`. Default: 1

- seed:

  (optional, integer) Random seed, required for reproducibility when
  using cross-validation or random forest models. Default: 1

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  `preference_order`).

## Value

dataframe:

- `response`: character, response name, if any, or `"none"` otherwise.

- `predictor`: character, name of the predictor.

- `f`: name of the function used to compute the preference order. If
  argument `f` is NULL, the value "stats::cor()" is added to this
  column.

- `metric`: name of the metric used to assess strength of association.
  Usually one of "R-squared", "AUC" (Area Under the ROC Curve), or
  "Cramer's V". If `f` is a custom function not in
  [`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md),
  then `metric` is set to "custom". If `f` is NULL, then "1 - R-squared"
  is returned in this column.

- `score`: value of the metric returned by `f` to assess the association
  between the `response` and each given `predictor`.

- `rank`: integer value indicating the rank of the predictor.

## See also

Other preference_order_functions:
[`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md),
[`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md),
[`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md),
[`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md),
[`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md),
[`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md),
[`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md),
[`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md),
[`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md),
[`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
#load example data
data(
  vi_smol,
  vi_predictors_numeric
)

##OPTIONAL: parallelization setup
# future::plan(
#   future::multisession,
#   workers = future::availableCores() - 1
# )

##OPTIONAL: progress bar
##does not work in R examples
# progressr::handlers(global = TRUE)

#ranking predictors from lower to higher multicollinearity
#------------------------------------------------
x <- preference_order(
  df = vi_smol,
  responses = NULL, #default value
  predictors = vi_predictors_numeric[1:10],
  f = NULL #must be explicit
)
#> 
#> collinear::preference_order(): ranking 10 'predictors' from lower to higher multicollinearity.

x
#>    response             predictor            f        metric  score rank
#> 1      none        topo_elevation stats::cor() 1 - R-squared 0.8184    1
#> 2      none        topo_diversity stats::cor() 1 - R-squared 0.8183    2
#> 3      none            topo_slope stats::cor() 1 - R-squared 0.8001    3
#> 4      none  soil_temperature_min stats::cor() 1 - R-squared 0.7434    4
#> 5      none             swi_range stats::cor() 1 - R-squared 0.7387    5
#> 6      none soil_temperature_mean stats::cor() 1 - R-squared 0.6534    6
#> 7      none              swi_mean stats::cor() 1 - R-squared 0.6272    7
#> 8      none               swi_max stats::cor() 1 - R-squared 0.6259    8
#> 9      none               swi_min stats::cor() 1 - R-squared 0.6170    9
#> 10     none  soil_temperature_max stats::cor() 1 - R-squared 0.6021   10

#automatic selection of ranking function
#------------------------------------------------
x <- preference_order(
  df = vi_smol,
  responses = c("vi_numeric", "vi_categorical"),
  predictors = vi_predictors_numeric[1:10],
  f = f_auto
  )
#> 
#> collinear::preference_order()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - vi_categorical
#> 
#> collinear::preference_order()
#> └── collinear::validate_arg_responses(): argument 'response' must be of length 1, using response: 
#>  - vi_numeric
#> 
#> collinear::preference_order(): processing response 'vi_numeric'.
#> ----------------------------------------------------------------
#> 
#> collinear::preference_order()
#> └── collinear::f_auto(): selected function 'f_numeric_glm()' to compute preference order.
#> 
#> collinear::preference_order(): processing response 'vi_categorical'.
#> --------------------------------------------------------------------
#> 
#> collinear::preference_order()
#> └── collinear::f_auto(): selected function 'f_categorical_rf()' to compute preference order.

x
#>          response             predictor                f     metric  score rank
#> 1      vi_numeric              swi_mean    f_numeric_glm  R-squared 0.7182    1
#> 2      vi_numeric               swi_max    f_numeric_glm  R-squared 0.5549    2
#> 3      vi_numeric  soil_temperature_max    f_numeric_glm  R-squared 0.5462    3
#> 4      vi_numeric             swi_range    f_numeric_glm  R-squared 0.4225    4
#> 5      vi_numeric               swi_min    f_numeric_glm  R-squared 0.2130    5
#> 6      vi_numeric  soil_temperature_min    f_numeric_glm  R-squared 0.0972    6
#> 7      vi_numeric        topo_diversity    f_numeric_glm  R-squared 0.0663    7
#> 8      vi_numeric        topo_elevation    f_numeric_glm  R-squared 0.0351    8
#> 9      vi_numeric            topo_slope    f_numeric_glm  R-squared 0.0176    9
#> 10     vi_numeric soil_temperature_mean    f_numeric_glm  R-squared 0.0151   10
#> 11 vi_categorical              swi_mean f_categorical_rf Cramer's V 0.5881    1
#> 12 vi_categorical  soil_temperature_max f_categorical_rf Cramer's V 0.5679    2
#> 13 vi_categorical               swi_max f_categorical_rf Cramer's V 0.5363    3
#> 14 vi_categorical             swi_range f_categorical_rf Cramer's V 0.4871    4
#> 15 vi_categorical               swi_min f_categorical_rf Cramer's V 0.4491    5
#> 16 vi_categorical soil_temperature_mean f_categorical_rf Cramer's V 0.4015    6
#> 17 vi_categorical  soil_temperature_min f_categorical_rf Cramer's V 0.3878    7
#> 18 vi_categorical        topo_elevation f_categorical_rf Cramer's V 0.3287    8
#> 19 vi_categorical        topo_diversity f_categorical_rf Cramer's V 0.1992    9
#> 20 vi_categorical            topo_slope f_categorical_rf Cramer's V 0.1150   10

#user selection of ranking function
#------------------------------------------------
#Poisson GLM for a integer counts response
x <- preference_order(
  df = vi_smol,
  responses = "vi_binomial",
  predictors = vi_predictors_numeric[1:10],
  f = f_binomial_glm
)

x
#>       response             predictor              f metric  score rank
#> 1  vi_binomial              swi_mean f_binomial_glm    AUC 0.8800    1
#> 2  vi_binomial  soil_temperature_max f_binomial_glm    AUC 0.8380    2
#> 3  vi_binomial             swi_range f_binomial_glm    AUC 0.7452    3
#> 4  vi_binomial  soil_temperature_min f_binomial_glm    AUC 0.7445    4
#> 5  vi_binomial               swi_min f_binomial_glm    AUC 0.7230    5
#> 6  vi_binomial               swi_max f_binomial_glm    AUC 0.7162    6
#> 7  vi_binomial        topo_diversity f_binomial_glm    AUC 0.6804    7
#> 8  vi_binomial            topo_slope f_binomial_glm    AUC 0.5975    8
#> 9  vi_binomial        topo_elevation f_binomial_glm    AUC 0.5508    9
#> 10 vi_binomial soil_temperature_mean f_binomial_glm    AUC 0.4432   10

#cross-validation
#------------------------------------------------
x <- preference_order(
  df = vi_smol,
  responses = "vi_binomial",
  predictors = vi_predictors_numeric[1:10],
  f = f_binomial_glm,
  cv_training_fraction = 0.5,
  cv_iterations = 10
)

x
#>       response             predictor              f metric  score rank
#> 1  vi_binomial              swi_mean f_binomial_glm    AUC 0.8811    1
#> 2  vi_binomial  soil_temperature_max f_binomial_glm    AUC 0.8357    2
#> 3  vi_binomial  soil_temperature_min f_binomial_glm    AUC 0.7505    3
#> 4  vi_binomial             swi_range f_binomial_glm    AUC 0.7434    4
#> 5  vi_binomial               swi_min f_binomial_glm    AUC 0.7229    5
#> 6  vi_binomial               swi_max f_binomial_glm    AUC 0.7174    6
#> 7  vi_binomial        topo_diversity f_binomial_glm    AUC 0.6845    7
#> 8  vi_binomial            topo_slope f_binomial_glm    AUC 0.5911    8
#> 9  vi_binomial        topo_elevation f_binomial_glm    AUC 0.5505    9
#> 10 vi_binomial soil_temperature_mean f_binomial_glm    AUC 0.4615   10

#custom pairwise correlation function
#------------------------------------------------
#custom functions need the ellipsis argument
f_rsquared <- function(df, ...){
    stats::cor(
      x = df$x,
      y = df$y,
      use = "complete.obs"
    )^2
}

x <- preference_order(
  df = vi_smol,
  responses = "vi_numeric",
  predictors = vi_predictors_numeric[1:10],
  f = f_rsquared
)

x
#>      response             predictor          f metric  score rank
#> 1  vi_numeric              swi_mean f_rsquared custom 0.7182    1
#> 2  vi_numeric               swi_max f_rsquared custom 0.5549    2
#> 3  vi_numeric  soil_temperature_max f_rsquared custom 0.5462    3
#> 4  vi_numeric             swi_range f_rsquared custom 0.4225    4
#> 5  vi_numeric               swi_min f_rsquared custom 0.2130    5
#> 6  vi_numeric  soil_temperature_min f_rsquared custom 0.0972    6
#> 7  vi_numeric        topo_diversity f_rsquared custom 0.0663    7
#> 8  vi_numeric        topo_elevation f_rsquared custom 0.0351    8
#> 9  vi_numeric            topo_slope f_rsquared custom 0.0176    9
#> 10 vi_numeric soil_temperature_mean f_rsquared custom 0.0151   10

#resetting to sequential processing
#future::plan(future::sequential)
```
