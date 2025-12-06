# Package index

## Multicollinearity Filtering

Remove redundant predictors from modelling datasets. These functions
filter variables by pairwise correlation, variance inflation factors, or
both, while respecting user-defined predictor priorities.

- [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
  : Smart multicollinearity management
- [`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md)
  : Dual multicollinearity filtering algorithm
- [`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
  : Multicollinearity filtering by pairwise correlation threshold
- [`step_collinear()`](https://blasbenito.github.io/collinear/reference/step_collinear.md)
  [`prep(`*`<step_collinear>`*`)`](https://blasbenito.github.io/collinear/reference/step_collinear.md)
  [`bake(`*`<step_collinear>`*`)`](https://blasbenito.github.io/collinear/reference/step_collinear.md)
  : Tidymodels recipe step for multicollinearity filtering
- [`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)
  : Multicollinearity filtering by variance inflation factor threshold

## Multicollinearity Assessment

Quantify redundancy among predictors. Compute pairwise correlations,
variance inflation factors, and summary statistics for datasets with
numeric and categorical variables.

- [`collinear_stats()`](https://blasbenito.github.io/collinear/reference/collinear_stats.md)
  : Compute summary statistics for correlation and VIF
- [`cor_clusters()`](https://blasbenito.github.io/collinear/reference/cor_clusters.md)
  : Group predictors by hierarchical correlation clustering
- [`cor_cramer()`](https://blasbenito.github.io/collinear/reference/cor_cramer.md)
  : Quantify association between categorical variables
- [`cor_df()`](https://blasbenito.github.io/collinear/reference/cor_df.md)
  : Compute signed pairwise correlations dataframe
- [`cor_matrix()`](https://blasbenito.github.io/collinear/reference/cor_matrix.md)
  : Signed pairwise correlation matrix
- [`cor_stats()`](https://blasbenito.github.io/collinear/reference/cor_stats.md)
  : Compute summary statistics for absolute pairwise correlations
- [`vif()`](https://blasbenito.github.io/collinear/reference/vif.md) :
  Compute variance inflation factors from a correlation matrix
- [`vif_df()`](https://blasbenito.github.io/collinear/reference/vif_df.md)
  : Compute variance inflation factors dataframe
- [`vif_stats()`](https://blasbenito.github.io/collinear/reference/vif_stats.md)
  : VIF Statistics

## Predictor Ranking

Prioritize predictors for multicollinearity filtering. Rank variables by
their association with a response or by their redundancy with other
predictors. Supports cross-validation and multiple response types.

- [`f_binomial_gam()`](https://blasbenito.github.io/collinear/reference/f_binomial_gam.md)
  : Area under the curve of binomial GAM predictions vs. observations

- [`f_binomial_glm()`](https://blasbenito.github.io/collinear/reference/f_binomial_glm.md)
  : Area Under the Curve of Binomial GLM predictions vs. observations

- [`f_binomial_rf()`](https://blasbenito.github.io/collinear/reference/f_binomial_rf.md)
  : Area Under the Curve of Binomial Random Forest predictions vs.
  observations

- [`f_categorical_rf()`](https://blasbenito.github.io/collinear/reference/f_categorical_rf.md)
  : Cramer's V of Categorical Random Forest predictions vs. observations

- [`f_count_gam()`](https://blasbenito.github.io/collinear/reference/f_count_gam.md)
  : R-squared of Poisson GAM predictions vs. observations

- [`f_count_glm()`](https://blasbenito.github.io/collinear/reference/f_count_glm.md)
  : R-squared of Poisson GLM predictions vs. observations

- [`f_count_rf()`](https://blasbenito.github.io/collinear/reference/f_count_rf.md)
  : R-squared of Random Forest predictions vs. observations

- [`f_numeric_gam()`](https://blasbenito.github.io/collinear/reference/f_numeric_gam.md)
  : R-squared of Gaussian GAM predictions vs. observations

- [`f_numeric_glm()`](https://blasbenito.github.io/collinear/reference/f_numeric_glm.md)
  : R-squared of Gaussian GLM predictions vs. observations

- [`f_numeric_rf()`](https://blasbenito.github.io/collinear/reference/f_numeric_rf.md)
  : R-squared of Random Forest predictions vs. observations

- [`preference_order()`](https://blasbenito.github.io/collinear/reference/preference_order.md)
  : Rank predictors by importance or multicollinearity

- [`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md)
  : Automatic selection of predictor scoring method

- [`f_auto_rules()`](https://blasbenito.github.io/collinear/reference/f_auto_rules.md)
  :

  Decision rules for
  [`f_auto()`](https://blasbenito.github.io/collinear/reference/f_auto.md)

- [`f_functions()`](https://blasbenito.github.io/collinear/reference/f_functions.md)
  : List predictor scoring functions

## Target Encoding

Convert categorical predictors to numeric using response values.
Implements mean, leave-one-out, and rank encoding methods for seamless
integration of categorical variables in correlation and VIF analyses.

- [`target_encoding_lab()`](https://blasbenito.github.io/collinear/reference/target_encoding_lab.md)
  : Convert categorical predictors to numeric via target encoding
- [`target_encoding_loo()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)
  [`target_encoding_mean()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)
  [`target_encoding_rank()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)
  : Encode categories as response means

## Example Data

Sample datasets for exploring package functionality. Includes dataframes
with numeric, categorical, and mixed predictor types, plus multiple
response encodings.

- [`toy`](https://blasbenito.github.io/collinear/reference/toy.md) : Toy
  dataframe with varying levels of multicollinearity

- [`vi`](https://blasbenito.github.io/collinear/reference/vi.md) : Large
  example dataframe

- [`vi_predictors`](https://blasbenito.github.io/collinear/reference/vi_predictors.md)
  :

  Vector of all predictor names in `vi` and `vi_smol`

- [`vi_predictors_categorical`](https://blasbenito.github.io/collinear/reference/vi_predictors_categorical.md)
  :

  Vector of categorical predictors in `vi` and `vi_smol`

- [`vi_predictors_numeric`](https://blasbenito.github.io/collinear/reference/vi_predictors_numeric.md)
  :

  Vector of numeric predictor names in `vi` and `vi_smol`

- [`vi_responses`](https://blasbenito.github.io/collinear/reference/vi_responses.md)
  :

  Vector of response names in `vi` and `vi_smol`

- [`vi_smol`](https://blasbenito.github.io/collinear/reference/vi_smol.md)
  : Small example dataframe

## Validation Experiments

Results from simulation studies used to calibrate adaptive thresholds
and validate the equivalence between correlation and VIF filtering.

- [`experiment_adaptive_thresholds`](https://blasbenito.github.io/collinear/reference/experiment_adaptive_thresholds.md)
  : Dataframe resulting from experiment to test the automatic selection
  of multicollinearity thresholds

- [`experiment_cor_vs_vif`](https://blasbenito.github.io/collinear/reference/experiment_cor_vs_vif.md)
  : Dataframe with results of experiment comparing correlation and VIF
  thresholds

- [`gam_cor_to_vif`](https://blasbenito.github.io/collinear/reference/gam_cor_to_vif.md)
  : GAM describing the relationship between correlation and VIF
  thresholds

- [`prediction_cor_to_vif`](https://blasbenito.github.io/collinear/reference/prediction_cor_to_vif.md)
  :

  Prediction of the model `gam_cor_to_vif` across correlation values

## Print and Summary Methods

S3 methods for displaying and summarizing results from
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)
and related functions.

- [`print(`*`<collinear_output>`*`)`](https://blasbenito.github.io/collinear/reference/print.collinear_output.md)
  :

  Print all collinear selection results of
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)

- [`print(`*`<collinear_selection>`*`)`](https://blasbenito.github.io/collinear/reference/print.collinear_selection.md)
  :

  Print single selection results from `collinear`

- [`summary(`*`<collinear_output>`*`)`](https://blasbenito.github.io/collinear/reference/summary.collinear_output.md)
  :

  Summarize all results of
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md)

- [`summary(`*`<collinear_selection>`*`)`](https://blasbenito.github.io/collinear/reference/summary.collinear_selection.md)
  :

  Summarize single response selection results of `collinear`

## Variable Type Detection

Identify and classify variables by type. Detect numeric, categorical,
logical, and near-zero variance columns in modelling datasets.

- [`identify_categorical_variables()`](https://blasbenito.github.io/collinear/reference/identify_categorical_variables.md)
  : Find valid categorical variables in a dataframe
- [`identify_logical_variables()`](https://blasbenito.github.io/collinear/reference/identify_logical_variables.md)
  : Find logical variables in a dataframe
- [`identify_numeric_variables()`](https://blasbenito.github.io/collinear/reference/identify_numeric_variables.md)
  : Find valid numeric variables in a dataframe
- [`identify_response_type()`](https://blasbenito.github.io/collinear/reference/identify_response_type.md)
  : Detect response variable type for model selection
- [`identify_valid_variables()`](https://blasbenito.github.io/collinear/reference/identify_valid_variables.md)
  : Find valid numeric, categorical, and logical variables in a
  dataframe
- [`identify_zero_variance_variables()`](https://blasbenito.github.io/collinear/reference/identify_zero_variance_variables.md)
  : Find near-zero variance variables in a dataframe

## Modelling Utilities

Helper functions for model fitting and evaluation. Generate formulas,
compute performance metrics, and create class-balancing weights.

- [`case_weights()`](https://blasbenito.github.io/collinear/reference/case_weights.md)
  : Generate sample weights for imbalanced responses
- [`model_formula()`](https://blasbenito.github.io/collinear/reference/model_formula.md)
  : Build model formulas from response and predictors
- [`score_auc()`](https://blasbenito.github.io/collinear/reference/score_auc.md)
  : Compute area under the ROC curve between binomial observations and
  probabilistic predictions
- [`score_cramer()`](https://blasbenito.github.io/collinear/reference/score_cramer.md)
  : Compute Cramer's V between categorical observations and predictions
- [`score_r2()`](https://blasbenito.github.io/collinear/reference/score_r2.md)
  : Compute R-squared between numeric observations and predictions

## Input Validation

Internal functions for checking and preparing function arguments. Ensure
data frames, variable names, and parameters meet requirements.

- [`drop_geometry_column()`](https://blasbenito.github.io/collinear/reference/drop_geometry_column.md)
  :

  Removes `geometry` Column From `sf` Dataframes

- [`validate_arg_df()`](https://blasbenito.github.io/collinear/reference/validate_arg_df.md)
  :

  Check and prepare argument `df`

- [`validate_arg_df_not_null()`](https://blasbenito.github.io/collinear/reference/validate_arg_df_not_null.md)
  :

  Ensure that argument `df` is not `NULL`

- [`validate_arg_encoding_method()`](https://blasbenito.github.io/collinear/reference/validate_arg_encoding_method.md)
  :

  Check and validate argument `encoding_method`

- [`validate_arg_f()`](https://blasbenito.github.io/collinear/reference/validate_arg_f.md)
  :

  Check and validate argument `f`

- [`validate_arg_function_name()`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md)
  : Build hierarchical function names for messages

- [`validate_arg_max_cor()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_cor.md)
  :

  Check and constrain argument `max_cor`

- [`validate_arg_max_vif()`](https://blasbenito.github.io/collinear/reference/validate_arg_max_vif.md)
  :

  Check and constrain argument `max_vif`

- [`validate_arg_predictors()`](https://blasbenito.github.io/collinear/reference/validate_arg_predictors.md)
  :

  Check and validate argument `predictors`

- [`validate_arg_preference_order()`](https://blasbenito.github.io/collinear/reference/validate_arg_preference_order.md)
  :

  Check and complete argument `preference_order`

- [`validate_arg_quiet()`](https://blasbenito.github.io/collinear/reference/validate_arg_quiet.md)
  :

  Check and validate argument `quiet`

- [`validate_arg_responses()`](https://blasbenito.github.io/collinear/reference/validate_arg_responses.md)
  :

  Check and validate arguments `response` and `responses`
