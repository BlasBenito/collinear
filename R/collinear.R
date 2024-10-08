#' @title Automated multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and categorical predictors by combining four methods:
#' \itemize{
#' \item Pairwise correlation filtering: Pearson, Spearman, and Cramer's V statistics to identify pairs of highly correlated predictors. See [cor_select()] and [cor_df()].
#' \item Variance Inflation Factor (VIF) filtering: identifies predictors that are linear combinations of other predictors. See [vif_select()] and [vif_df()].
#' \item Target encoding: to transform categorical predictors to numeric using a numeric variable as reference. See [target_encoding_lab()].
#' \item Flexible prioritization method: to help the user select a meaningful set of non-correlated predictors. See [preference_order()].
#' }
#'
#' See *Details* for a deeper explanation on each method implemented here.
#'
#'
#' @section Variance Inflation Factor:
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all other predictors in the input data frame as predictors. The VIF equation can be interpreted as the "rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' @section VIF-based Filtering:
#'
#' The function [vif_select()] computes Variance Inflation Factors and removes variables iteratively, until all variables in the resulting selection have a VIF below `max_vif`.
#'
#' If the argument `preference_order` is not provided, all variables are first ranked from lower to higher VIF, as returned by [vif_df()]., and the variable with a higher VIF above `max_vif` is removed on each iteration.
#'
#' If `preference_order` is defined, whenever two or more variables are above `max_vif`, the one higher in `preference_order` is preserved, and the next one with a higher VIF is removed.
#'
#' @section Pairwise Correlation Filtering:
#'
#' The function [cor_select()] applies a recursive forward selection algorithm to keep predictors with a maximum Pearson correlation with all other selected predictors lower than `max_cor`.
#'
#' If the argument `preference_order` is not provided, the variables are ranked from lower to higher sum of absolute pairwise correlation with all other variables. When two variables are correlated above `max_cor`, the one with the lowest preference order is removed.
#'
#' @section Target Encoding:
#'
#' When the `response` argument is provided, any non-numeric predictors are converted to numeric via target encoding with the function [target_encoding_lab()], to facilitate the handling of all predictors during the multicollinearity filtering.
#'
#' When the 'response' argument is not provided, categorical variables are ignored. However, in such case, the function [cor_select()] can handle categorical variables, albeit with a lower computation speed.
#'
#' @section Preference Order:
#'
#' The argument `preference_order` allows prioritizing variables that might be interesting or even required for a given analysis. If `preference_order` is not provided, then the predictors are ranked from lower to higher sum of their absolute correlations with the other predictors in [cor_select()], and by their VIF in [vif_select()], and removed one by one until the maximum R-squared of the correlation matrix is lower than `max_cor` and the maximum VIF is below `max_vif`.
#'
#'
#' @param df (required; data frame, tibble, or sf) A data frame with numeric predictors, and optionally a numeric response and categorical predictors. Default: NULL.
#' @param response (optional, character string) Name of a numeric (non-numerics are ignored) response variable in `df`, only required if there are categorical variables named in the argument `predictors`. Default: NULL.
#' @param predictors (optional; character vector) Names of the variables to select from `df`. If omitted, all numeric columns in `df` are used instead. If argument `response` is not provided, non-numeric variables are ignored. Default: NULL
#' @param preference_order (optional; character vector) Variable names in `predictors`. Defines a priority order, from first to last, to preserve variables during the selection process. Variables not included in this argument are ranked by their Variance Inflation Factor. See [preference_order()]. Default: NULL
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Either "pearson" or "spearman". Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of variables in `predictors`. Recommended values are between 0.5 and 0.9. Higher values return larger number of predictors with a higher multicollinearity. Default: `0.75`
#' @param max_vif (optional, numeric) Maximum Variance Inflation Factor allowed during variable selection. Recommended values are between 2.5 and 10. Higher values return larger number of predictors with a higher multicollinearity. Default: 5.
#' @param encoding_method (optional; character string). Name of the target encoding method to transform non-numeric variables to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#' @return character vector; selected predictor names
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
    attributes(predictors) <- NULL
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
