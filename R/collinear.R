#' @title Automated multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and non-numeric predictors by combining four methods:
#' \itemize{
#' \item **Target Encoding**: When a numeric `response` is provided and `encoding_method` is not NULL, it transforms categorical predictors (classes "character" and "factor") to numeric using the response values as reference. See [target_encoding_lab()] for further details.
#' \item **Preference Order**: When a response of any type is provided via `response`, the association between the response and each predictor is computed with an appropriate function (see [preference_order()] and [f_default()]), and all predictors are ranked from higher to lower association. This rank is used to preserve important predictors during the multicollinearity filtering.
#' \item **Pairwise Correlation Filtering**: Automated multicollinearity filtering via pairwise correlation. Correlations between numeric and categoricals  predictors are computed by target-encoding the categorical against the predictor, and correlations between categoricals are computed via Cramer's V. See [cor_select()], [cor_df()], and [cramer_v()] for further details.
#' \item **VIF filtering**: Automated algorithm to identify and remove numeric predictors that are linear combinations of other predictors. See [vif_select()] and [vif_df()].
#' }
#'
#' @section Target Encoding:
#'
#' When the argument `response` names a **numeric response variable**, categorical predictors in `predictors` (or in the columns of `df` if `predictors` is NULL) are converted to numeric via **target encoding** with the function [target_encoding_lab()]. When `response` is NULL or names a categorical variable, target-encoding is skipped. This feature facilitates multicollinearity filtering in data frames with mixed column types.
#'
#' @section Preference Order:
#'
#' This feature is designed to help protect important predictors during the multicollinearity filtering. It involves the arguments `preference_order` and `f`.
#'
#' The argument `preference_order` accepts:
#' \itemize{
#'   \item: A character vector of predictor names in a custom order of preference, from first to last. This vector does not need to contain all predictor names, but only the ones relevant to the user.
#'   \item A data frame returned by [preference_order()], which ranks predictors based on their association with a response variable.
#'   \item If NULL, and `response` is provided, then [preference_order()] is used internally to rank the predictors using the function `f`. If `f` is NULL as well, then [f_default()] selects a proper function based on the data properties.
#' }
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
#' @param encoding_method (optional; character string). Name of the target encoding method. One of: "mean" (default), "rank", "loo". If NULL, target encoding is disabled. Default: "mean"
#' @param preference_order (optional; character vector) Variable names in `predictors`. Defines a priority order, from first to last, to preserve variables during the selection process. Variables not included in this argument are ranked by their Variance Inflation Factor. See [preference_order()]. Default: NULL
#' @param f (optional: function) Function to compute preference order. If NULL (default), the output of [f_default()] for the given data is used:
#' \itemize{
#'   \item [f_auc_rf()]: `response` is binomial.
#'   \item [f_r2_pearson()]: `response` and `predictors` are numeric.
#'   \item [f_v()]: `response` and `predictors` are categorical.
#'   \item [f_v_rf_categorical()]: `response` is categorical and `predictors` are numeric or mixed .
#'   \item [f_r2_rf()]: in all other cases.
#' }
#' Default: NULL
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Either "pearson" or "spearman". Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of variables in `predictors`. Recommended values are between 0.5 and 0.9. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the pairwise correlation analysis is disabled. Default: `0.75`
#' @param max_vif (optional, numeric) Maximum Variance Inflation Factor allowed during variable selection. Recommended values are between 2.5 and 10. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the variance inflation analysis is disabled. Default: 5.
#' @return character vector; names of selected predictors
#'
#' @examples
#'
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #subset to limit example run time
#' df <- vi[1:1000, ]
#' predictors <- vi_predictors[1:10]
#'
#' #predictors has mixed types
#' sapply(
#'   X = df[, predictors],
#'   FUN = class
#' )
#'
#' #without response
#' #--------------------------------
#' #  no target encoding
#' #  no preference order
#' #  all predictors filtered by correlation
#' #  only numerics filtered by VIF
#' x <- collinear(
#'   df = df,
#'   predictors = predictors,
#'   max_cor = 0.75, #default
#'   max_vif = 5     #default
#'   )
#'
#' x
#'
#' #all correlations below max_cor
#' cor_df(
#'   df = df,
#'   predictors = x
#' )
#'
#' #all vif below max vif
#' #ignores categoricals
#' vif_df(
#'   df = df,
#'   predictors = x
#' )
#'
#'
#' #with numeric response
#' #--------------------------------
#'
#' #  target encoding
#' #  automated preference order
#' #  all predictors filtered by correlation and VIF
#' x <- collinear(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors
#' )
#'
#' x
#'
#' #disabling target encoding
#' # x <- collinear(
#' #   df = vi,
#' #   response = "vi_numeric",
#' #   predictors = predictors_mixed,
#' #   encoding_method = NULL
#' # )
#' #
#' # x
#'
#' #with custom preference order
#' x <- collinear(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_type",
#'     "koppen_zone"
#'   )
#' )
#'
#' x
#'
#' #with quantitative preference order
#' preference_df <- preference_order(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors
#' )
#'
#' x <- collinear(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors,
#'   preference_order = preference_df
#' )
#'
#' x
#'
#' #with binomial response
#' #--------------------------------
#'
#' #  target encoding
#' #  automated preference order (different f function)
#' #  all predictors filtered by correlation and VIF
#' x <- collinear(
#'   df = df,
#'   response = "vi_binomial",
#'   predictors = predictors
#' )
#'
#' x
#'
#'
#' #with counts response
#' #--------------------------------
#'
#' #  target encoding
#' #  automated preference order (different f function)
#' #  all predictors filtered by correlation and VIF
#' x <- collinear(
#'   df = df,
#'   response = "vi_counts",
#'   predictors = predictors
#' )
#'
#' x
#'
#' #with categorical response
#' #--------------------------------
#'
#' #  target encoding
#' #  automated preference order (different f function)
#' # all predictors filtered by correlation
#' # numeric predictors filtered by VIF
#' x <- collinear(
#'   df = df,
#'   response = "vi_category",
#'   predictors = predictors
#' )
#'
#' x
#'
#' #resetting to sequential processing
#' future::plan(future::sequential)
#'
#' @autoglobal
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
    encoding_method = "mean",
    preference_order = NULL,
    f = NULL,
    cor_method = "pearson",
    max_cor = 0.75,
    max_vif = 5
){

  #validate input data frame
  df <- validate_df(
    df = df
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

  #transform categorical to numerics
  #ignores if:
  # response == NULL
  # encoding_method == NULL
  df <- target_encoding_lab(
    df = df,
    response = response,
    predictors = predictors,
    encoding_methods = encoding_method,
    replace = TRUE,
    verbose = FALSE
  )

  #preference order
  if(is.null(preference_order)){

    preference_order <- preference_order(
      df = df,
      response = response,
      predictors = predictors,
      f = NULL
    )

  }

  #pairwise correlation filter
  predictors <- validate_data_cor(
    df = df,
    predictors = predictors,
    function_name = "collinear::collinear()"
  )

  selection.cor <- cor_select(
    df = df,
    predictors = predictors,
    preference_order = preference_order,
    cor_method = cor_method,
    max_cor = max_cor
  )

  #separate numeric and categorical
  selection.cor.type <- identify_predictors(
    df = df,
    predictors = selection.cor
  )

  #vif filter
  predictors.vif <- validate_data_vif(
    df = df,
    predictors = selection.cor.type$numeric,
    function_name = "collinear::collinear()"
  )

  selection.vif <- vif_select(
    df = df,
    predictors = predictors.vif,
    preference_order = preference_order,
    max_vif = max_vif
  )

  #merge selections
  selection <- c(
    selection.vif,
    selection.cor.type$categorical
  ) |>
    unique() |>
    na.omit()

  #order as in preference order
  if(!is.null(preference_order)){

    if(is.data.frame(preference_order)){
      preference_order <- preference_order$predictor
    }

    selection <- selection[order(match(selection, preference_order))]

  }

  selection

}
