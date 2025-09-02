#' @title Automated Multicollinearity Management
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and non-numeric predictors by combining four methods:
#' \itemize{
#' \item **Target Encoding**: When a numeric \code{response} is provided and \code{encoding_method} is not NULL, it transforms categorical predictors (classes "character" and "factor") to numeric using the response values as reference. See [target_encoding_lab()] for further details.
#' \item **Preference Order**: When a response of any type is provided via \code{response}, the association between the response and each predictor is computed with an appropriate function (see [preference_order()] and [f_auto()]), and all predictors are ranked from higher to lower association. This rank is used to preserve important predictors during the multicollinearity filtering.
#' \item **Pairwise Correlation Filtering**: Automated multicollinearity filtering via pairwise correlation. Correlations between numeric and categoricals  predictors are computed by target-encoding the categorical against the predictor, and correlations between categoricals are computed via Cramer's V. See [cor_select()], [cor_df()], and [cor_cramer_v()] for further details.
#' \item **VIF filtering**: Automated algorithm to identify and remove numeric predictors that are linear combinations of other predictors. See [vif_select()] and [vif_df()].
#' }
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()]. If available, parallelization is used to speed-up the execution of [target_encoding_lab()], [preference_order()], and [cor_select()].
#'
#' Accepts a character vector of response variables as input for the argument \code{response}. When more than one response is provided, the output is a named list of character.
#'
#' @section Target Encoding:
#'
#' When the argument \code{response} names a numeric column in \code{df} and \code{encoding_method} is not NULL, categorical predictors in \code{predictors} (or in the columns of \code{df} if \code{predictors} is NULL) are converted to numeric via **target encoding** with the function [target_encoding_lab()]. When \code{response} is NULL or names a categorical variable, target-encoding is skipped. This feature enables multicollinearity filtering in data frames with mixed column types.
#'
#' @section Preference Order:
#'
#' This feature is designed to help protect important predictors during the multicollinearity filtering. It involves the arguments \code{preference_order} and \code{f}.
#'
#' The argument \code{preference_order} accepts:
#' \itemize{
#'   \item: A character vector of predictor names in a custom order of preference, from first to last. This vector does not need to contain all predictor names, but only the ones relevant to the user.
#'   \item A data frame returned by [preference_order()], which ranks predictors based on their association with a response variable.
#'   \item If NULL, and \code{response} is provided, then [preference_order()] is used internally to rank the predictors using the function \code{f}. If \code{f} is NULL as well, then [f_auto()] selects a proper function based on the data properties.
#' }
#'
#' @section Variance Inflation Factors:
#'
#' The Variance Inflation Factor for a given variable \eqn{a} is computed as \eqn{1/(1-R2)}, where \eqn{R2} is the multiple R-squared of a multiple regression model fitted using \eqn{a} as response and all other predictors in the input data frame as predictors, as in  \eqn{a = b + c + ...}.
#'
#' The square root of the VIF of \eqn{a} is the factor by which the confidence interval of the estimate for \eqn{a} in the linear model \eqn{y = a + b + c + ...} is widened by multicollinearity in the model predictors.
#'
#' The range of VIF values is (1, Inf]. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' @section VIF-based Filtering:
#'
#' The function [vif_select()] computes Variance Inflation Factors and removes variables iteratively, until all variables in the resulting selection have a VIF below \code{max_vif}.
#'
#' If the argument \code{preference_order} is not provided, all variables are ranked from lower to higher VIF, as returned by [vif_df()], and the variable with the higher VIF above \code{max_vif} is removed on each iteration.
#'
#' If \code{preference_order} is defined, whenever two or more variables are above \code{max_vif}, the one with a smaller index in the vector \code{preference_order} is preserved, and the next one with a higher VIF is removed. For example, for the predictors and preference order \eqn{a} and \eqn{b}, if any of their VIFs is higher than \code{max_vif}, then \eqn{b} will be removed regardless of whether its VIF is lower or higher than \eqn{a}'s VIF. If their VIF scores are lower than \code{max_vif}, then both are preserved.
#'
#' @section Pairwise Correlation Filtering:
#'
#' The function [cor_select()] applies a recursive forward selection algorithm to keep predictors with a maximum absolute Pearson correlation with all other selected predictors less than or equal to \code{max_cor}.
#'
#' If the argument \code{preference_order} is NULL, the predictors are ranked from lower to higher sum of absolute pairwise correlation with all other predictors.
#'
#' If \code{preference_order} is defined, whenever two or more variables are above \code{max_cor}, the one higher in \code{preference_order} is preserved. For example, for the predictors and preference order \eqn{a} and \eqn{b}, if their correlation is higher than \code{max_cor}, then \eqn{b} will be removed and \eqn{a} preserved. If their correlation is lower than \code{max_cor}, then both are preserved.
#'
#'
#' @param df (required; data frame, tibble, or sf) A data frame with responses (optional) and predictors. Must have at least 10 rows for pairwise correlation analysis, and \code{10 * (length(predictors) - 1)} for VIF analysis.  Default: NULL.
#'
#' @param response (optional; character, character vector, or NULL) Name of one or several response variables in \code{df}. When \code{encoding_method} is not NULL, response/s are used as reference to map categorical predictors, if any, to numeric (see [target_encoding_lab()]). When \code{f} is not NULL, responses are used to rank predictors and preserve important ones during multicollinearity filtering (see [preference_order()]). If no response is provided, the predictors are ranked from lower to higher multicollinearity. When several responses are provided, the selection results are named after each response in the output list. If no response is provided, the variable selection shows with the name "result" in the output list. Default: NULL.
#'
#' @param predictors (optional; character vector or NULL) Names of the predictors in \code{df} involved in the multicollinearity filtering. If NULL, all columns in \code{df} (except those with constant values or near zero variance) are used. Default: NULL
#'
#' @param encoding_method (optional; character or NULL). Name of one target encoding method. One of: "loo", "mean", or "rank" (see [target_encoding_lab()] for further details). If NULL, target encoding is disabled. Default: NULL
#'
#' @param preference_order (optional; character vector, output of [preference_order()], or NULL). Incompatible with \code{f} (overrides it when provided). Prioritizes predictors to preserve the most relevant ones during multicollinearity filtering.
#' Accepted inputs are:
#' \itemize{
#'   \item **NULL** (default): If argument \code{f} is NULL (default), predictors are ranked from lower to higher multicollinearity. Otherwise, [preference_order()] ranks the predictors according to their relationship with \code{response} using the function defined in \code{f}. NOTE: The output of this setting might differ to an external call to [preference_order()] if target encoding is triggered.
#'   \item **character vector**: Predictor names in a user-defined priority order. The first predictor in this vector is always selected, unless it has near zero-variance values. This option sets \code{f} to NULL.
#'   \item **data frame**: output of [preference_order()] computed on the given \code{response}. This option sets \code{f} to NULL.
#'   \item **named list**: list of data frames, output of [preference_order()] when argument \code{response} is a vector of length two or more. This option sets \code{f} to NULL.
#' }. Default: NULL
#'
#' @param f (optional: unquoted function name or NULL). Incompatible with \code{preference_order} (overridden if \code{preference_order} is provided). Function to rank \code{predictors} depending on their relationship with the \code{response}. Available functions are listed by [f_functions()] and described in the manual of [preference_order()]. Setting it to [f_auto] is a good starting point. Default: NULL
#'
#' @param max_cor (optional; numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. Valid values are between 0.01 and 0.99, and recommended values are between 0.5 (strict) and 0.9 (permissive). If NULL, the pairwise correlation analysis is disabled. Default: 0.75
#'
#' @param max_vif (optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2 (strict) and 10 (permissive). If NULL, the variance inflation analysis is disabled. Default: 5.
#'
#' @param quiet (optional; logical) If FALSE, messages are printed to the console. Default: FALSE
#'
#' @return list of class [collinear_output]
#'
#' @examples
#'   data(
#'     vi_smol,
#'     vi_predictors_numeric
#'   )
#'
#'   ##OPTIONAL: parallelization setup
#'   # future::plan(
#'   #   future::multisession,
#'   #   workers = 2
#'   # )
#'
#'   ##OPTIONAL: progress bar
#'   ##does not work in R examples
#'   #progressr::handlers(global = TRUE)
#'
#'   ##minimal setup
#'   ##--------------------------
#'   ##all columns in df are filtered
#'   ##uses numeric columns only to speed up example
#'   x <- collinear(
#'     df = vi_smol[, vi_predictors_numeric]
#'   )
#'
#'   names(x)
#'
#'   #print full object
#'   print(x)
#'
#'   #print selection only
#'   summary(x)
#'
#'   #get selection vector
#'   x$result$selection
#'
#'   #validated arguments are stored as well
#'   x$arguments
#'
#'   ##using predictors
#'   ##--------------------------
#'   ## - numeric predictors only
#'   ## - ordered by their mutual collinearity
#'   ## - not enough rows for a full VIF analysis
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric
#'   )
#'
#'
#'   ##disable VIF analysis
#'   ##--------------------------
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric,
#'     max_vif = NULL
#'   )
#'
#'
#'   ##disable correlation analysis
#'   ##--------------------------
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric,
#'     max_cor = NULL
#'   )
#'
#'
#'   ##automatic preference order
#'   ##--------------------------
#'   ## - rank predictors by R-squared with response (see f_functions() for more options)
#'   x <- collinear(
#'     df = vi_smol,
#'     response = "vi_numeric",
#'     predictors = vi_predictors_numeric,
#'     f = f_r2_pearson
#'   )
#'
#'   ##results for the given response
#'   x$vi_numeric
#'
#'   ##response name is used instead of 'result'
#'   x$vi_numeric$selection
#'
#'   ##formula for linear model
#'   x$vi_numeric$formulas$linear
#'
#'   ##formula for gam model
#'   x$vi_numeric$formulas$smooth
#'
#'   ##data frame with selected columns
#'   colnames(x$vi_numeric$df)
#'
#'   #preference dataframe in results
#'   x$vi_numeric$preference$df
#'
#'
#'   ##manual preference order
#'   ##--------------------------
#'   ## - rank predictors by order in vector 'preference_order'
#'   x <- collinear(
#'     df = vi_smol,
#'     response = "vi_numeric",
#'     predictors = vi_predictors_numeric,
#'     preference_order = c(
#'       "swi_mean",
#'       "soil_temperature_mean",
#'       "growing_season_length",
#'       "rainfall_mean"
#'     )
#'   )
#'
#'   #preference dataframe in results
#'   x$vi_numeric$preference$df
#'
#'   ##missing predictors in preference_order due to multicollinearity with predictors with a higher preference
#'   ##predictors not in preference order ranked by their collinearity with other predictors
#'   summary(x)
#'
#'
#'   ##several responses
#'   ##--------------------------------
#'   ## - automatic selection of preference order function with f_auto()
#'   x <- collinear(
#'     df = vi_smol,
#'     response = c(
#'       "vi_numeric",
#'       "vi_binomial"
#'     ),
#'     predictors = vi_predictors_numeric,
#'     f = f_auto
#'   )
#'
#'   ##one object per response
#'   x$vi_numeric$selection
#'   x$vi_binomial$selection
#'
#'   ##function arguments
#'   x$arguments
#'
#'   #resetting to sequential processing
#'   #future::plan(future::sequential)
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
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = FALSE
){

  function_name <- "collinear::collinear()"

  # VALIDATE ARGS ----
  args <- validate_args_collinear(
    df = df,
    response = response,
    predictors = predictors,
    encoding_method = encoding_method,
    preference_order = preference_order,
    f = f,
    f_name = deparse(substitute(f)),
    max_cor = max_cor,
    max_vif = max_vif,
    quiet = quiet,
    function_name = function_name
  )

  #manage response for loop
  if(
    length(args$response) == 0 ||
    is.null(args$response)
  ){
    responses <- list(NULL)
  } else {
    responses <- args$response
  }


  #if several responses, invalidate predictors
  if(length(responses) > 1){
    attributes(args$predictors)$validated <- NULL
  }


  #ITERATION ----
  out <- list()

  ## start ----
  for(response in responses){

    ### validate response ----
    if(is.list(response)){
      response <- NULL
    }

    response <- validate_arg_response(
      df = args$df,
      response = response,
      quiet = args$quiet,
      function_name = function_name
    )

    if(
      args$quiet == FALSE &&
      length(responses) > 1
    ){

      msg <- paste0(
        function_name,
        ": processing response '",
        response,
        "'"
      )

      msg_length <- nchar(msg)

      message("\n", msg)
      message(rep(x = "-", times = nchar(msg)))

    }

    ## copy df ----
    df.response <- args$df

    ## validate predictors ----
    predictors.response <- validate_arg_predictors(
      df = df.response,
      response = response,
      predictors = args$predictors,
      function_name = function_name
    )


    ## TARGET ENCODING ----
    if(
      !is.null(response) &&
      !is.null(args$encoding_method)
    ){

      df.response <- target_encoding_lab(
        df = df.response,
        response = response,
        predictors = predictors.response,
        encoding_method = args$encoding_method,
        overwrite = TRUE,
        quiet = args$quiet
      )

    }

    ## PREFERENCE ORDER ----
    preference_order.response <- preference_order_wrapper(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = args$preference_order,
      f = args$f,
      f_name = args$f_name,
      function_name = function_name,
      quiet = args$quiet
    )

    ##MULTICOLLINEARITY ANALYSIS ----

    ### empty selection ----
    selection.response <- predictors.response

    ### correlation ----
    if(!is.null(args$max_cor)){

      selection.response <- cor_select(
        df = df.response,
        predictors = predictors.response,
        preference_order = preference_order.response,
        max_cor = args$max_cor,
        quiet = args$quiet
      )

    }

    ### vif ----
    if(!is.null(args$max_vif)){

      #separate numeric and categorical
      selection.response.type <- identify_predictors(
        df = df.response,
        predictors = selection.response
      )

      selection.vif <- vif_select(
        df = df.response,
        predictors = selection.response.type$numeric,
        preference_order = preference_order.response,
        max_vif = args$max_vif,
        quiet = args$quiet
      )

      selection.response <- c(
        selection.vif,
        selection.response.type$categorical
      )

    }

    ##ORDER SELECTION ----
    if(length(selection.response) > 0){

      selection_in_preference_order <- intersect(
        x = preference_order.response$predictor,
        y = selection.response
      )

      selection_no_in_preference_order <- setdiff(
        x = selection.response,
        y = preference_order.response$predictor
      )

      selection.response <- c(
        selection_in_preference_order,
        selection_no_in_preference_order
      )

      df.response <- df.response[
        ,
        c(response, selection.response),
        drop = FALSE
      ]

      attr(
        x = df.response,
        which = "validated"
      ) <- TRUE


    } else {

      if(args$quiet == FALSE){

        message(
          "\n",
          function_name,
          ": no predictors selected."
        )

      }

      selection.response <- NULL

    }


    ## response list ----
    out.response <- build.collinear_selection(
      df = df.response,
      response = response,
      preference_order = preference_order.response,
      selection = selection.response,
      quiet = args$quiet
    )

    #store in output list
    if(is.null(response)){
      out[["result"]] <- out.response
    } else {
      out[[response]] <- out.response
    }

    # end ----
  } #end of loop

  #add arguments
  out$arguments <- args

  class(out) <- c(
    class(out),
    "collinear_output"
  )

  out

}
