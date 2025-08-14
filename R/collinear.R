#' @title Automated multicollinearity management
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
#' @param df (required; data frame, tibble, or sf) A data frame with responses and predictors. Must have at least 10 rows for pairwise correlation analysis, and \code{10 * (length(predictors) - 1)} for VIF analysis.  Default: NULL.
#' @param response (optional; character, character vector, or NULL) Name of one or several response variables in \code{df}, used to target encode categorical predictors, if any, when \code{encoding_method} is not NULL, and to compute preference order when \code{preference_order} is not NULL. If \code{response} is NULL, target encoding and preference order computation are disabled. Default: NULL.
#' @param predictors (optional; character vector or NULL) Names of the predictors to select from \code{df}. If omitted, all numeric columns in \code{df} are used instead. Default: NULL
#' @param encoding_method (optional; character or NULL). Name of one target encoding method or NULL. One of: "loo", "mean", or "rank" (see [target_encoding_lab()] for further details). If NULL, target encoding is disabled. Default: NULL
#' @param preference_order (optional; character vector, output of [preference_order()], list, or NULL). Prioritizes predictors to preserve the most relevant ones during multicollinearity filtering. Accepted inputs are:
#' \itemize{
#'   \item **NULL** (default): If argument \code{f} is NULL (default), predictors are prioritized from lower to higher multicollinearity. Otherwise it calls [preference_order()] to rank the predictors using the function defined in the argument \code{f}. The output of this setting might differ to an external call to [preference_order()] if target encoding is triggered to transform ategorical predictors to numeric.
#'   \item **character vector**: Predictor names in a user-defined priority order. The first predictor in this vector is always selected, unless it has near zero-variance values.
#'   \item **data frame**: output of [preference_order()] computed on the value of the argument \code{response}.
#'   \item **named list**: list of data frames, output of [preference_order()] when argument \code{response} is a vector of length two or more.
#' }. Default: NULL
#' @param f (optional: function name or NULL) Function to compute preference order used when \code{preference_order = "auto"}. Available functions are listed by [f_functions()] and described in the manual of [preference_order()]. If NULL, calls to [f_auto()] (see [f_auto_rules()]) to select a suitable method depending on the nature of the data. Default: NULL
#' @param max_cor (optional; numeric or NULL) Maximum correlation allowed between any pair of variables in \code{predictors}. Valid values are between 0.01 and 0.99, while recommended values are between 0.5 (strict) and 0.9 (permissive). Higher values return larger number of predictors with a higher multicollinearity. If NULL, the pairwise correlation analysis is disabled. Default: 0.75
#' @param max_vif (optional, numeric or NULL) Maximum Variance Inflation Factor allowed during variable selection. Recommended values are between 2 (strict) and 10 (permissive). Higher values return larger number of predictors with a higher multicollinearity. If NULL, the variance inflation analysis is disabled. Default: 5.
#' @param quiet (optional; logical) If FALSE, messages are printed to the console Default: FALSE
#'
#' @return list
#' @return A list whose structure depends on the value of the \code{response} argument:
#'
#' \itemize{
#'   \item If \code{response} is \code{NULL} or has length 1: an object of class [collinear_output] with a custom print method [print.collinear_output]. It contains:
#'     \itemize{
#'       \item \code{response} (\code{character} or \code{NULL}): Name of the response variable.
#'       \item \code{predictors} (\code{character} or \code{NULL}): Names of predictors considered in multicollinearity filtering.
#'       \item \code{selection} (\code{character} or \code{NULL}): Names of selected, non-collinear predictors.
#'       \item \code{df} (\code{data.frame} or \code{NULL}): Data frame including columns in \code{response} and \code{selection}. If target encoding was applied, categorical variables will be numeric.
#'       \item \code{arguments} (\code{list}): A list of arguments used in the call: \code{encoding_method}, \code{preference_order}, \code{f}, \code{max_cor}, and \code{max_vif}
#'     }
#'
#'   \item If \code{response} is a character vector of length >= 2: an object of class [collinear_list], a named list of \code{"collinear_output"} objects, one per response variable.
#' }

#'

#'
#' @examples
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar (does not work in examples)
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
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = FALSE
){

  function_name <- "collinear::collinear()"

  # VALIDATE ARGS ----

  ## max_cor and max_vif
  if(all(is.null(c(max_cor, max_vif)))){
    stop(
      function_name,
      ": arguments 'max_cor' and 'max_vif' cannot be NULL at once.",
      call. = FALSE
    )
  }

  ## quiet ----
  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  ## df ----
  df <- validate_arg_df(
    df = df,
    response = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## f ----
  if(!is.null(f)){

    f <- validate_arg_f(
      f = f,
      f_name = deparse(substitute(f)),
      function_name = function_name
    )

  }

  ## responses ----
  responses <- intersect(
    x = colnames(df),
    y = response
  )

  if(
    length(responses) == 0 ||
    is.null(responses)
  ){
    responses <- list(NULL)
  }

  ## avoid repeating messages in loop ----
  if(quiet == FALSE){

    if(is.null(encoding_method)){

      message(
        "\n",
        function_name,
        ": argument 'encoding_method' is NULL, skipping target encoding."
      )

    } else if(is.null(response)){

      message(
        "\n",
        function_name,
        ": argument 'response' is NULL, skipping target encoding."
      )

    }


    if(is.null(f)){

      message(
        "\n",
        function_name,
        ": argument 'f' is NULL, skipping computation of preference order."
      )

    }

    if(is.null(max_cor)){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is NULL, skipping correlation filtering."
      )

    }

    if(is.null(max_vif)){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is NULL, skipping skipping VIF filtering."
      )

    }

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
      df = df,
      response = response,
      quiet = quiet,
      function_name = function_name
    )

    if(
      quiet == FALSE &&
      length(responses) > 1
    ){

      msg <- paste0(
        function_name,
        ": processing response '",
        response,
        "'."
      )

      msg_length <- nchar(msg)

      message("\n", msg)
      message(rep(x = "-", times = nchar(msg)))

    }

    ## copy df ----
    df.response <- df

    ## validate predictors ----
    predictors.response <- validate_arg_predictors(
      df = df.response,
      response = response,
      predictors = predictors,
      function_name = function_name
    )


    ## TARGET ENCODING ----
    if(
      !is.null(response) &&
      !is.null(encoding_method)
    ){

      encoding_method <- encoding_method[1]

      df.response <- target_encoding_lab(
        df = df.response,
        response = response,
        predictors = predictors.response,
        methods = encoding_method,
        overwrite = TRUE,
        quiet = quiet
      )

    }

    ## PREFERENCE ORDER ----
    preference_order.response <- preference_order_wrapper(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = preference_order,
      f = f,
      quiet = quiet
    )

    ##MULTICOLLINEARITY ANALYSIS ----

    ### empty selection ----
    selection.response <- predictors.response

    ### correlation ----
    if(!is.null(max_cor)){

      selection.response <- cor_select(
        df = df.response,
        predictors = predictors.response,
        preference_order = preference_order.response,
        max_cor = max_cor,
        quiet = quiet
      )

    }

    ### vif ----
    if(!is.null(max_vif)){

      #separate numeric and categorical
      selection.response.type <- identify_predictors(
        df = df.response[, selection.response],
        predictors = selection.response
      )

      selection.vif <- vif_select(
        df = df.response,
        predictors = selection.response.type$numeric,
        preference_order = preference_order.response,
        max_vif = max_vif,
        quiet = quiet
      )

      selection.response <- c(
        selection.vif,
        selection.response.type$categorical
      )

    }

    ##ORDER SELECTION ----
    if(length(selection.response) > 0){

      selection_in_preference_order <- intersect(
        x = preference_order.response,
        y = selection.response
      )

      selection_no_in_preference_order <- setdiff(
        x = selection.response,
        y = preference_order.response
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

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": no predictors selected."
        )

      }

      selection.response <- NULL

    }


    ## OUTPUT LIST ----

    ### list main ----
    out.response <- list(
      response = response,
      predictors = predictors.response,
      selection = selection.response,
      df = df.response,
      arguments = list(
        encoding_method = encoding_method,
        preference_order = preference_order.response,
        f = attributes(preference_order.response)$f_name,
        max_cor = max_cor,
        max_vif = max_vif
      )
    )

    ### formulas ----
    if(
      all(
        c(
          !is.null(response),
          !is.null(selection.response),
          !is.null(df.response)
        )
      )
    ){

      response_type <- identify_predictors(
        df = df.response,
        predictors = response
      ) |>
        unlist() |>
        names()

      selection_type <- identify_predictors(
        df = df.response,
        predictors = selection.response
      )

      out.response$formulas <- list()

      #general formula
      general_formula <- model_formula(
        df = df.response,
        response = response,
        predictors = selection.response,
        quiet = quiet
      )

      #general formula name
      general_formula_name <- c(
        numeric = "linear",
        logical = "binomial",
        categorical = "classification"
      )[[response_type]]

      out.response$formulas[[general_formula_name]] <- general_formula

      #add smooth formula if relevant
      if(
        response_type == "numeric" &&
        length(selection_type$numeric) > 0
      ){

        out.response$formulas[["smooth"]] <- model_formula(
          df = df.response,
          response = response,
          predictors = selection.response,
          term_f = "s",
          quiet = quiet
        )

      }

    }

    ## timestamp ----
    out.response$timestamp = Sys.time()

    ## class ----
    class(out.response) <- c(
      class(out.response),
      "collinear_output"
    )

    #store in output list
    if(is.null(response) || length(responses) == 1){
      out <- out.response
    } else {
      out[[response]] <- out.response
    }

    # end ----
  } #end of loop

  out

}
