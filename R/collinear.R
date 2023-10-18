#' @title Autommated multicollinearity reduction
#'
#' @description
#'
#' The `collinear()` function is designed to automate the reduction of multicollinearity in a set of predictors by sequentially applying correlation-based and VIF-based variable selection by applying [cor_select()] and [vif_select()] sequentially.
#'
#' If the 'response' argument is provided, categorical predictors are converted to numeric via target encoding (see [target_encoding_lab()]). If the 'response' argument is not provided, categorical variables are ignored.
#'
#' The function [cor_select()] applies a recursive algorithm to remove variables with a Pearson correlation with another variable higher than a given threshold (defined by the argument `max_cor`).  When two variables are correlated above this threshold, the one with the highest sum of R-squared with all the other variables is removed.
#'
#' The function [vif_select()] applyies a Variance Inflation Factor (VIF) analysis to reduce multicollinearity. The VIF for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data frame as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model". The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The argument `preference_order` allows the user to "protect" variables that might be interesting or even required for the given analysis.
#'
#' If `preference_order` is not provided, then the predictors are ranked from lower to higher sum of R-squared with the other preodictors, and removed one by one until the maximum R-squared of the correlation matrix is lower than `max_cor` and the maximum VIF is below `max_vif`.
#'
#' Please note that near-zero variance columns are ignored by this function.
#'
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default: `NULL`
#' @param preference_order  (optional; character vector) vector with column names in 'predictors' in the desired preference order, or result of the function `preference_order()`. Allows defining a priority order for selecting predictors, which can be particularly useful when some predictors are more critical for the analysis than others. Predictors not included in this argument are ranked by their Variance Inflation Factor. Default: `NULL`.
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of predictors. Higher values return larger number of predictors with higher multicollinearity. Default: `0.75`
#' @param max_vif (optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed for any given predictor in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: `5`.
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: `"mean"`
#' @return Character vector with the names of uncorrelated predictors.
#'
#' @examples
#' if(interactive()){

#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
#'
#' #no response
#' #no preference_order
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
#' #no response
#' #no preference_order
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
#' #no preference_order
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


#' }
#' @autoglobal
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
