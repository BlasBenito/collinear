#' @title Automated multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and categorical predictors by combining four methods:
#' \itemize{
#' \item Pairwise correlation filtering: Pearson, Spearman, and Cramer's V statistics to identify pairs of highly correlated predictors.
#' \item Variance Inflation Factor (VIF) filtering: identifies predictors that are linear combinations of other predictors.
#' \item Target encoding: to transform categorical predictors to numeric using a numeric variable as reference.
#' \item Flexible prioritization method: to help the user select a meaningful set of non-correlated predictors.
#' }
#'
#' The pairwise correlation filtering is implemented in [cor_select()]. This function applies a recursive forward selection algorithm to keep predictors with a Pearson correlation with all other selected predictors lower than the value of the argument `max_cor`.  When two predictors are correlated above this threshold, the one with the lowest preference order is removed. At this stage, if `preference_order` is NULL, predictors are ranked from lower to higher sum of absolute pairwise correlation with the other predictors.
#'
#' The VIF-based filtering is implemented in [vif_select()], which removes variables and recomputes VIF scores iteratively, until all variables in the resulting selection have a VIF below the value of the argument `max_vif`. The VIF for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the R-squared of a multiple regression model fitted using `y` as response against the other predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model". The possible range of VIF values is (1, Inf], but the recommended thresholds for maximum VIF (argument `max_vif`) may vary, being 2.5, 5, and 10 the values most commonly mentioned in the relevant bibliography. At this stage, if `preference_order` is NULL, predictors are ranked from lower to higher Variance Inflation Factor.
#'
#' When a 'response' argument is provided, categorical predictors are converted to numeric via target encoding with the function [target_encoding_lab()], and all predictors are then handled as numeric during the multicollinearity filtering. When the 'response' argument is not provided, categorical variables are ignored. However, in such case, the function [cor_select()] can handle categorical variables, albeit with a lower computation speed.
#'
#' The argument `preference_order` allows prioritizing variables that might be interesting or even required for a given analysis. If `preference_order` is not provided, then the predictors are ranked from lower to higher sum of their absolute correlations with the other predictors in [cor_select()], and by their VIF in [vif_select()], and removed one by one until the maximum R-squared of the correlation matrix is lower than `max_cor` and the maximum VIF is below `max_vif`.
#'
#' Please note that near-zero variance columns are identified by [identify_zero_variance_predictors()], and ignored by [collinear()], [cor_select()], and [vif_select()].
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default: NULL
#' @param preference_order  (optional; character vector) vector with column names in 'predictors' in the desired preference order, or result of the function [preference_order()]. Allows defining a priority order for selecting predictors, which can be particularly useful when some predictors are more critical for the analysis than others. Predictors not included in this argument are ranked by their Variance Inflation Factor. Default: NULL.
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of predictors. Higher values return larger number of predictors with higher multicollinearity. Default: `0.75`
#' @param max_vif (optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed for any given predictor in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#' @return Character vector with the names of uncorrelated predictors.
#'
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #without response
#' #without preference_order
#' #permissive max_cor and max_vif
#' #only numeric variables in output
#' selected.predictors <- collinear(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_cor = 0.8,
#'   max_vif = 10
#'   )
#'
#' selected.predictors
#'
#' #without response
#' #without preference_order
#' #restrictive max_cor and max_vif
#' #only numeric variables in output
#' selected.predictors <- collinear(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_cor = 0.5,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #without preference_order
#' #restrictive max_cor and max_vif
#' #numerics and categorical variables in output
#' selected.predictors <- collinear(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   max_cor = 0.5,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #with user-defined preference_order
#' #restrictive max_cor and max_vif
#' #numerics and categorical variables in output
#' selected.predictors <- collinear(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   preference_order = c(
#'     "soil_temperature_mean",
#'     "swi_mean",
#'     "rainfall_mean",
#'     "evapotranspiration_mean"
#'   ),
#'   max_cor = 0.5,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#'
#' #with response
#' #with automated preference_order
#' #restrictive max_cor and max_vif
#' #numerics and categorical variables in output
#' preference.order <- preference_order(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   f = f_rsquared, #cor(response, predictor)
#'   workers = 1
#' )
#'
#' selected.predictors <- collinear(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   preference_order = preference.order,
#'   max_cor = 0.5,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' @autoglobal
#' @author Blas M. Benito
#' @references
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. \doi{10.1002/0471725153}.
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32 \doi{10.1145/507533.507538}
#' }
#' @export
collinear <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    cor_method = "pearson",
    max_cor = 0.75,
    max_vif = 5,
    encoding_method = "mean"
){

  #validate input data frame
  df <- validate_df(
    df = df,
    min_rows = ifelse(
      test = cor_method == "pearson",
      yes = 30,
      no = 10
    )
  )

  #validate predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors,
    min_numerics = 0
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
  )

  #target encode character predictors
  df <- target_encoding_lab(
    df = df,
    response = response,
    predictors = predictors,
    encoding_methods = encoding_method,
    replace = TRUE,
    verbose = FALSE
  )

  #use only numeric predictors if the response is NULL
  if(is.null(response)){
    predictors <- identify_numeric_predictors(
      df = df,
      predictors = predictors
    )
  }

  #applying cor selection
  selected.predictors <- cor_select(
    df = df,
    response = response,
    predictors = predictors,
    preference_order = preference_order,
    cor_method = cor_method,
    max_cor = max_cor
  )

  #applying vif selection
  selected.predictors <- vif_select(
    df = df,
    response = response,
    predictors = selected.predictors,
    preference_order = preference_order,
    max_vif = max_vif
  )

  selected.predictors


}
