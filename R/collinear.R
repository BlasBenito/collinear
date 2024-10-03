#' @title Automated multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and non-numeric predictors by combining four methods:
#' \itemize{
#' \item **Target Encoding**: transforms non-numeric predictors to numeric using another numeric variable (usually a model response) as reference. See [target_encoding_lab()].
#' \item **Preference Order**: method to rank and preserve relevant variables during  multicollinearity filtering. See [preference_order()].
#' \item **Pairwise Correlation Filtering**: automated logic to identify pairs of highly correlated variables. See [cor_select()] and [cor_df()].
#' \item **VIF-based filtering**: to identify and remove predictors that are linear combinations of other predictors. See [vif_select()] and [vif_df()].
#' }
#'
#' This function calls these other functions:
#' \itemize{
#'   \item [target_encoding_lab()]: to apply target encoding, if `response` is provided and there are categorical variables named in `predictors`.
#'   \item [cor_select()]: to apply the automated pairwise correlation filtering.
#'   \item [vif_select()]: to apply the automated VIF-based filtering.
#' }
#'
#'
#'
#' @section Target Encoding:
#'
#' When the name of a numeric response variable is provided via the `response` argument, categorical predictors in `predictors` (or in the columns of `df` if `predictors` is NULL) are converted to numeric via **target encoding** with the function [target_encoding_lab()]. When `response` is NULL, [collinear()] (and [vif_select()]) ignore categorical predictors. This feature facilitates multicollinearity filtering in data frames with mixed column types.
#'
#' @section Preference Order:
#'
#' The argument `preference_order` helps answer the question "what variable should I remove first?". Such answer is provided as a character vector of predictor names in the order of preference (from first to last) in which they want to be preserved. This vector does not need to name all predictors in `predictors`, but only the relevant ones for the user.
#'
#' The function [preference_order()] helps defining a quantitative criteria for preference order by assessing the relationship between each variable in `predictors` with a response variable.
#' If the argument `preference order` is not provided, [cor_select()] ranks the predictors by the sum of their absolute pairwise correlations with the other predictors, and [vif_select()] ranks them from lower to higher VIF.
#'
#'
#' @section Variance Inflation Factors:
#'
#' The Variance Inflation Factor for a given variable \eqn{a} is computed as \eqn{1/(1-R2)}, where \eqn{R2} is the multiple R-squared of a multiple regression model fitted using \eqn{a} as response and all other predictors in the input data frame as predictors, as in  \eqn{a = b + c + ...}.
#'
#' The square root of the VIF of \eqn{a} is the factor by which the confidence interval of the estimate for \eqn{a} in the linear model \eqn{y = a + b + c + ...}` is widened by multicollinearity in the model predictors.
#'
#' The range of VIF values is (1, Inf]. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' @section VIF-based Filtering:
#'
#' The function [vif_select()] computes Variance Inflation Factors and removes variables iteratively, until all variables in the resulting selection have a VIF below `max_vif`.
#'
#' If the argument `preference_order` is not provided, all variables are ranked from lower to higher VIF, as returned by [vif_df()], and the variable with the higher VIF above `max_vif` is removed on each iteration.
#'
#' If `preference_order` is defined, whenever two or more variables are above `max_vif`, the one higher in `preference_order` is preserved, and the next one with a higher VIF is removed. For example, for the predictors and preference order \eqn{a} and \eqn{b}, if any of their VIFs is higher than `max_vif`, then \eqn{b} will be removed no matter whether its VIF is lower or higher than \eqn{a}'s VIF. If their VIF scores are lower than `max_vif`, then both are preserved.
#'
#' @section Pairwise Correlation Filtering:
#'
#' The function [cor_select()] applies a recursive forward selection algorithm to keep predictors with a maximum Pearson correlation with all other selected predictors lower than `max_cor`.
#'
#' If the argument `preference_order` is not provided, the predictors are ranked from lower to higher sum of absolute pairwise correlation with all other predictors.
#'
#' If `preference_order` is defined, whenever two or more variables are above `max_cor`, the one higher in `preference_order` is preserved. For example, for the predictors and preference order \eqn{a} and \eqn{b}, if their correlation is higher than `max_cor`, then \eqn{b} will be removed and \eqn{a} preserved. If their correlation is lower than `max_cor`, then both are preserved.
#'
#'
#' @param df (required; data frame, tibble, or sf) A data frame with numeric predictors, and optionally a numeric response and categorical predictors. Default: NULL.
#' @param response (optional, character string) Name of a numeric (non-numerics are ignored) response variable in `df`, only required if there are categorical variables named in the argument `predictors`. Default: NULL.
#' @param predictors (optional; character vector) Names of the variables to select from `df`. If omitted, all numeric columns in `df` are used instead. If argument `response` is not provided, non-numeric variables are ignored. Default: NULL
#' @param preference_order (optional; character vector) Variable names in `predictors`. Defines a priority order, from first to last, to preserve variables during the selection process. Variables not included in this argument are ranked by their Variance Inflation Factor. See [preference_order()]. Default: NULL
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Either "pearson" or "spearman". Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of variables in `predictors`. Recommended values are between 0.5 and 0.9. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the pairwise correlation analysis is disabled. Default: `0.75`
#' @param max_vif (optional, numeric) Maximum Variance Inflation Factor allowed during variable selection. Recommended values are between 2.5 and 10. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the variance inflation analysis is disabled. Default: 5.
#' @param encoding_method (optional; character string). Name of the target encoding method to transform non-numeric variables to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). If NULL, target encoding is ignored, and categorical predictors are ignored during the VIF analysis. Default: "mean"
#' @return character vector; names of selected predictors
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
#' @author Blas M. Benito, PhD
#' @references
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. DOI: 10.1002/0471725153.
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32. DOI: 10.1145/507533.507538
#' }
#' @family automated_multicollinearity_analysis
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

  response <- validate_response(
    df = df,
    response = response
  )

  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors
  )

  #early output if only one predictor
  if(length(predictors) == 1){
    return(predictors)
  }

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
