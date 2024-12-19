#' @title Automated multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and non-numeric predictors by combining four methods:
#' \itemize{
#' \item **Target Encoding**: When a numeric `response` is provided and `encoding_method` is not NULL, it transforms categorical predictors (classes "character" and "factor") to numeric using the response values as reference. See [target_encoding_lab()] for further details.
#' \item **Preference Order**: When a response of any type is provided via `response`, the association between the response and each predictor is computed with an appropriate function (see [preference_order()] and [f_auto()]), and all predictors are ranked from higher to lower association. This rank is used to preserve important predictors during the multicollinearity filtering.
#' \item **Pairwise Correlation Filtering**: Automated multicollinearity filtering via pairwise correlation. Correlations between numeric and categoricals  predictors are computed by target-encoding the categorical against the predictor, and correlations between categoricals are computed via Cramer's V. See [cor_select()], [cor_df()], and [cor_cramer_v()] for further details.
#' \item **VIF filtering**: Automated algorithm to identify and remove numeric predictors that are linear combinations of other predictors. See [vif_select()] and [vif_df()].
#' }
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' Accepts a character vector of response variables as input for the argument `response`. When more than one response is provided, the output is a named list of character.
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
#'   \item If NULL, and `response` is provided, then [preference_order()] is used internally to rank the predictors using the function `f`. If `f` is NULL as well, then [f_auto()] selects a proper function based on the data properties.
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
#' If the argument `preference_order` is NULL, the predictors are ranked from lower to higher sum of absolute pairwise correlation with all other predictors.
#'
#' If `preference_order` is defined, whenever two or more variables are above `max_cor`, the one higher in `preference_order` is preserved. For example, for the predictors and preference order \eqn{a} and \eqn{b}, if their correlation is higher than `max_cor`, then \eqn{b} will be removed and \eqn{a} preserved. If their correlation is lower than `max_cor`, then both are preserved.
#'
#'
#' @param df (required; data frame, tibble, or sf) A data frame with responses and predictors. Default: NULL.
#' @param response (optional; character string or vector) Name/s of response variable/s in `df`. Used in target encoding when it names a numeric variable and there are categorical predictors, and to compute preference order. Default: NULL.
#' @param predictors (optional; character vector) Names of the predictors to select from `df`. If omitted, all numeric columns in `df` are used instead. If argument `response` is not provided, non-numeric variables are ignored. Default: NULL
#' @param encoding_method (optional; character string). Name of the target encoding method. One of: "loo", "mean", or "rank". If NULL, target encoding is disabled. Default: "loo"
#' @param preference_order (optional; string, character vector, output of [preference_order()]). Defines a priority order, from first to last, to preserve predictors during the selection process. Accepted inputs are:
#' \itemize{
#'   \item **"auto"** (default): if `response` is not NULL, calls [preference_order()] for internal computation.
#'   \item **character vector**: predictor names in a custom preference order.
#'   \item **data frame**: output of [preference_order()] from `response` of length one.
#'   \item **named list**: output of [preference_order()] from `response` of length two or more.
#'   \item **NULL**: disabled.
#' }. Default: "auto"
#' @param f (optional: function) Function to compute preference order. If "auto" (default) or NULL, the output of [f_auto()] for the given data is used:
#' \itemize{
#'   \item [f_auc_rf()]: if `response` is binomial.
#'   \item [f_r2_pearson()]: if `response` and `predictors` are numeric.
#'   \item [f_v()]: if `response` and `predictors` are categorical.
#'   \item [f_v_rf_categorical()]: if `response` is categorical and `predictors` are numeric or mixed .
#'   \item [f_r2_rf()]: in all other cases.
#' }
#' Default: NULL
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of variables in `predictors`. Recommended values are between 0.5 and 0.9. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the pairwise correlation analysis is disabled. Default: `0.75`
#' @param max_vif (optional, numeric) Maximum Variance Inflation Factor allowed during variable selection. Recommended values are between 2.5 and 10. Higher values return larger number of predictors with a higher multicollinearity. If NULL, the variance inflation analysis is disabled. Default: 5.
#' @param quiet (optional; logical) If FALSE, messages generated during the execution of the function are printed to the console Default: FALSE
#'
#' @return
#' \itemize{
#'   \item character vector if `response` is NULL or is a string.
#'   \item named list if `response` is a character vector.
#' }
#'
#' @examples
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar
#' #progressr::handlers(global = TRUE)
#'
#' #subset to limit example run time
#' df <- vi[1:500, ]
#'
#' #predictors has mixed types
#' #small subset to speed example up
#' predictors <- c(
#'   "swi_mean",
#'   "soil_type",
#'   "soil_temperature_mean",
#'   "growing_season_length",
#'   "rainfall_mean"
#'   )
#'
#'
#' #with numeric responses
#' #--------------------------------
#' #  target encoding
#' #  automated preference order
#' #  all predictors filtered by correlation and VIF
#' x <- collinear(
#'   df = df,
#'   response = c(
#'     "vi_numeric",
#'     "vi_binomial"
#'     ),
#'   predictors = predictors
#' )
#'
#' x
#'
#'
#' #with custom preference order
#' #--------------------------------
#' x <- collinear(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_type"
#'   )
#' )
#'
#'
#' #pre-computed preference order
#' #--------------------------------
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
    encoding_method = "loo",
    preference_order = "auto",
    f = "auto",
    max_cor = 0.75,
    max_vif = 5,
    quiet = FALSE
){

  if(!is.logical(quiet)){
    message("\ncollinear::collinear(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  #validate df
  df <- validate_df(
    df = df[ , c(response, predictors)],
    quiet = quiet
  )

  #managing multiple responses
  responses <- intersect(
    x = colnames(df),
    y = response
  )

  #reorder responses
  responses <- na.omit(responses[match(response, responses)])
  rm(response)

  #copy of preference order to avoid overwriting in loop
  preference_order_user <- preference_order

  #output list
  out <- list()

  #iterate over responses
  #runs even when responses is NULL
  for(
    response in
    if (is.null(responses)) list(NULL) else responses
  ){

    if(is.list(response)){
      response <- NULL
    }

    #validate response
    response <- validate_response(
      df = df,
      response = response,
      quiet = quiet
    )

    if(!is.null(response)){

      if(quiet == FALSE){

        message("\ncollinear::collinear(): processing response '", response, "'." )
        message("---------------------------------------------------------------")

      }

    }

    #reset and validate predictors
    predictors.response <- validate_predictors(
      df = df,
      response = response,
      predictors = predictors
    )

    # target encoding ----
    #ignored if:
    # response == NULL
    # encoding_method == NULL
    df.response <- target_encoding_lab(
      df = df,
      response = response,
      predictors = predictors.response,
      methods = encoding_method[1],
      overwrite = TRUE,
      quiet = quiet
    )

    #compute preference order
    preference_order <- preference_order_collinear(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = preference_order_user,
      f = f,
      quiet = quiet
    )

    # correlation filter ----
    selection.cor <- cor_select(
      df = df.response,
      predictors = predictors.response,
      preference_order = preference_order,
      max_cor = max_cor,
      quiet = quiet
    )

    # vif filter ----

    #selection by numeric and categorial
    #this section lets vif_select() yield a message
    #about the categorical variables it is ignoring
    if(is.null(max_cor)){

      selection.cor.type <- list(
        numeric = selection.cor,
        categorical = NULL
      )

    } else {

      #separate numeric and categorical
      selection.cor.type <- identify_predictors(
        df = df.response,
        predictors = selection.cor
      )

    }

    #run vif filtering
    selection.vif <- vif_select(
      df = df.response,
      predictors = selection.cor.type$numeric,
      preference_order = preference_order,
      max_vif = max_vif,
      quiet = quiet
    )

    #merge selections
    selection <- c(
      selection.vif,
      selection.cor.type$categorical
    ) |>
      unique() |>
      na.omit()

    #order as in preference order
    if(all(selection %in% preference_order)){

      selection <- selection[order(match(selection, preference_order))]

    }

    #message with selection if required
    if(
      (
        length(selection.cor.type$categorical) > 0 &&
        !is.null(max_vif) &&
        any(!selection.cor.type$numeric %in% selection.vif)
      ) &&
      quiet == FALSE
    ){

      message(
        "\ncollinear::collinear(): selected predictors: \n - ",
        paste(selection, collapse = "\n - ")
      )

    }

    #add validated
    attr(
      x = selection,
      which = "validated"
    ) <- TRUE

    if(!is.null(response)){

      attributes(response) <- NULL

      attr(
        x = selection,
        which = "response"
      ) <- response

      if(length(responses) > 1){
        out[[response]] <- selection
      } else {
        out <- selection
      }

    } else {
      out <- selection
    }

    rm(selection)

  } #end of loop

  out

}
