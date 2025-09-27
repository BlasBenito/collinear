#' @title Comprehensive Multicollinearity Management
#'
#' @description
#'
#' Automates multicollinearity management by combining four methods:
#'
#' \itemize{
#'
#'   \item **Target Encoding**: When \code{responses} is numeric, there are categorical variables in \code{predictors}, and \code{encoding_method} is one of "loo", "mean", or "rank" (see [target_encoding_lab()] for further details), categorical predictors are remapped to numeric using the response values as reference. This feature enables multicollinearity filtering in data frames with mixed column types.
#'
#'   \item **Preference Order**: System to rank predictors and protect important ones during multicollinearity filtering. The function offers three alternative options:
#'
#'   \itemize{
#'
#'     \item Argument \code{preference_order}: Accepts a character vector of predictor names ranked from left to right, or a result from [preference_order()]. When two predictors in this vector or dataframe are highly collinear, the one with a lower ranking is removed. This option helps the user focus the analysis on particular predictors of interest.
#'
#'    \item Argument \code{f}: Takes an unquoted function name (see output of [f_functions()]) that is used by [preference_order()] to rank predictors by their association with the response. Alternatively, the function [f_auto] chooses an appropriate \code{f} function depending on the nature of the response and the predictors. Using this option helps preserve those predictors with a stronger relationship with the \code{responses}, and results in stronger statistical models.
#'
#'    \item When \code{preference_order} and \code{f} are NULL, predictors are ranked from lower to higher multicollilnearity. This option preserves rare predictors over redundant ones, but does not guarantee strong statistical models.
#'   }
#'
#'   \item **Pairwise Correlation Filtering**: Computes the correlation between all pairs of predictors and removes redundant ones while taking preference order into account. Correlations between numeric and categorical predictors are assessed by target-encoding the categorical predictor against the numeric one and computing their Pearson correlation. Correlations between pairs of categorical predictors are computed with Cramer's V. Pearson correlation and Cramer's V are not directly comparable, but this function assumes they are. See [cor_select()], [cor_df()], and [cor_cramer_v()] for further details.
#'
#'    \item **VIF-based Filtering**: Computes Variance Inflation Factors for numeric predictors and removes redundant ones iteratively, while taking preference order into account. See [vif()], [vif_df()] and [vif_select()] for further details.
#' }
#'
#' This function accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()]. Parallelization speeds-up the execution of [target_encoding_lab()], [preference_order()], and [cor_select()]. This setup is generally not worth it for small data frames with numeric predictors only.
#'
#'
#' @section Variance Inflation Factors:
#'
#' The Variance Inflation Factor of a given predictor \eqn{a} is computed as \eqn{1/(1-R2)}, where \eqn{R2} is the multiple R-squared of a multiple regression model fitted using \eqn{a} as response against all other predictors, as in  \eqn{a = b + c + ...}.
#'
#' The square root of the VIF of \eqn{a} is the factor by which the confidence interval of the estimate for \eqn{a} in the linear model \eqn{y = a + b + c + ...} is widened by multicollinearity in the predictors.
#'
#' The range of VIF values is (1, Inf]. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5 (most conservative), 5, and 10 (most permissive).
#'
#' @section VIF-based Filtering:
#'
#' This algorithm, implemented in [vif_select()], selects numeric predictors with a Variance Inflation Factor lower than \code{max_vif} while preserving those with a higher ranking in \code{preference_order}, if provided.
#'
#' The algorithm works as follows:
#'
#' \enumerate{
#'
#'   \item All numeric predictors are ranked according to the argument \code{preference_order}, or from lower to higher VIF with the other predictors (as computed by [vif_df()]) otherwise. Categorical predictors are ignored.
#'
#'   \item The predictor with the higher rank is added to the selection.
#'
#'   \item The VIF of the next predictor in the ranking against the predictor/s in the selection is computed via [cor_matrix()] and [vif()]. If the resulting VIF is lower than \code{max_vif}, the new predictor is added to the selection. Otherwise it is removed.
#'
#'   \item Step 3 is repeated until all predictors are processed.
#'
#' }
#'
#' This filtering method ensures that all predictors in the selection are correlated below \code{max_vif} and that the selection respects the prioritization defined in \code{preference_order}.
#'
#' @section Pairwise Correlation Filtering:
#'
#' This algorithm is implemented in [cor_select()], and selects numeric and categorical predictors with Pearson correlation or Cramer's V association lower than \code{max_cor} while preserving those with a higher ranking in \code{preference_order}, if provided.
#'
#' It works as follows:
#'
#' \enumerate{
#'
#'   \item The correlation matrix of all predictors is computed, its diagonals are set to zero, and the rows and columns are ordered according to \code{preference_order}, or from lower to higher sum of correlations with the other predictors (([colSums()] applied to the output of [cor_matrix()])) otherwise.
#'
#'   \item The predictor with the higher rank is added to the selection.
#'
#'   \item The correlation/s between the predictor/s in the selection and the next predictor in the ranking is/are extracted from the global correlation matrix. If the maximum correlation is below the value of \code{max_cor}, the new predictor is added to the selection. Otherwise it is removed.
#'
#'   \item Step 3 is repeated until all predictors are processed.
#'
#' }
#'
#' This filtering method ensures that all predictors in the selection are correlated below \code{max_cor} and that the selection respects the prioritization defined in \code{preference_order}.
#'
#'
#' @param df (required; data frame, tibble, or sf) A data frame with responses (optional) and predictors. Must have at least 10 rows for pairwise correlation analysis, and \code{10 * (length(predictors) - 1)} for VIF analysis.  Default: NULL.
#'
#' @param responses (optional; character, character vector, or NULL) Name of one or several response variables in \code{df}. When \code{encoding_method} is not NULL, response/s are used as reference to map categorical predictors, if any, to numeric (see [target_encoding_lab()]). When \code{f} is not NULL, responses are used to rank predictors and preserve important ones during multicollinearity filtering (see [preference_order()]). If no response is provided, the predictors are ranked from lower to higher multicollinearity. When several responses are provided, the selection results are named after each response in the output list. If no response is provided, the variable selection shows with the name "result" in the output list. Default: NULL.
#'
#' @param predictors (optional; character vector or NULL) Names of the predictors in \code{df} involved in the multicollinearity filtering. If NULL, all columns in \code{df} (except those with constant values or near zero variance) are used. Default: NULL
#'
#' @param encoding_method (optional; character or NULL). Name of one target encoding method. One of: "loo", "mean", or "rank" (see [target_encoding_lab()] for further details). If NULL, target encoding is disabled. Default: NULL
#'
#' @param preference_order (optional; character vector, output of [preference_order()], or NULL). Incompatible with \code{f} (overrides it when provided). Prioritizes predictors to preserve the most relevant ones during multicollinearity filtering.
#'
#' Accepted inputs are:
#'
#' \itemize{
#'
#'   \item **NULL** (default): If argument \code{f} is NULL (default), predictors are ranked from lower to higher multicollinearity. Otherwise, [preference_order()] ranks the predictors according to their relationship with \code{responses} using the function defined in \code{f}. NOTE: The output of this setting might differ to an external call to [preference_order()] if target encoding is triggered.
#'
#'   \item **character vector**: Predictor names in a user-defined priority order. The first predictor in this vector is always selected, unless it has near zero-variance values. This option sets \code{f} to NULL.
#'
#'   \item **data frame**: output of [preference_order()] computed on the given \code{responses}. This option sets \code{f} to NULL.
#'
#'   \item **named list**: list of data frames, output of [preference_order()] when argument \code{responses} is a vector of length two or more. This option sets \code{f} to NULL.
#' }. Default: NULL
#'
#' @param f (optional: unquoted function name or NULL). Incompatible with \code{preference_order} (overridden if \code{preference_order} is provided). Function to rank \code{predictors} depending on their relationship with the \code{responses}. Available functions are listed by [f_functions()] and described in the manual of [preference_order()]. Setting it to [f_auto] is a good starting point. Default: NULL
#'
#' @param max_cor (optional; numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. Valid values are between 0.01 and 0.99, and recommended values are between 0.5 (strict) and 0.9 (permissive). If NULL, the pairwise correlation analysis is disabled. Default: 0.7
#'
#' @param max_vif (optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2.5 (strict) and 10 (permissive). If NULL, the variance inflation analysis is disabled. Default: 5.
#'
#' @param quiet (optional; logical) If FALSE, messages are printed to the console. Default: FALSE
#'
#' @param ... (optional) Other arguments. Currently used for the internal argument \code{function_name}.
#'
#' @return list of class \code{class.collinear_output}
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
#'     responses = "vi_numeric",
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
#'     responses = "vi_numeric",
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
#'   ##missing predictors in preference_order
#'   ##due to multicollinearity with
#'   ##predictors with a higher preference
#'   ##predictors not in preference order
#'   ##ranked by their collinearity
#'   ##with other predictors
#'   summary(x)
#'
#'
#'   ##several responses
#'   ##--------------------------------
#'   ## - automatic selection of preference order function with f_auto()
#'   x <- collinear(
#'     df = vi_smol,
#'     responses = c(
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
    responses = NULL,
    predictors = NULL,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    max_cor = 0.70,
    max_vif = 5,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear()",
    ... = ...
  )

  # VALIDATE ARGS ----
  args <- class.collinear_arguments(
    df = df,
    responses = responses,
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
    length(args$responses) == 0 ||
    is.null(args$responses)
  ){
    responses <- list(NULL)
  } else {
    responses <- args$responses
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
        quiet = args$quiet,
        function_name = function_name
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
      quiet = args$quiet,
      function_name = function_name
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
        quiet = args$quiet,
        function_name = function_name
      )

    }

    ### vif ----
    if(!is.null(args$max_vif)){

      #separate numeric and categorical
      selection.response.type <- identify_predictors(
        df = df.response,
        predictors = selection.response,
        function_name = function_name
      )

      if(length(selection.response.type$numeric) > 1){

        #filter preference order
        if(is.data.frame(preference_order.response)){

          preference_order.response.vif <- preference_order.response[
            preference_order.response$predictor %in% selection.response.type$numeric,
            ]

        } else if(is.character(preference_order.response)){

          preference_order.response.vif <- intersect(
            x = preference_order.response,
            y = selection.response.type$numeric
          )

        } else {

          preference_order.response.vif <- preference_order.response

        }

        selection.vif <- vif_select(
          df = df.response,
          predictors = selection.response.type$numeric,
          preference_order = preference_order.response.vif,
          max_vif = args$max_vif,
          quiet = args$quiet,
          function_name = function_name
        )

        selection.response <- c(
          selection.vif,
          selection.response.type$categorical
        )

      } else {

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": no numeric predictors available for VIF filtering, skipping it."
          )

        }

      }

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
    out.response <- class.collinear_selection(
      df = df.response,
      response = response,
      preference_order = preference_order.response,
      selection = selection.response,
      quiet = args$quiet,
      function_name = function_name
    )

    #store in output list
    if(is.null(response)){
      out[["result"]] <- out.response
    } else {
      out[[response]] <- out.response
    }

    # end ----
  } #end of loop

  out_list <- class.collinear_output(
    collinear_selection = out,
    collinear_arguments = args
  )

  out_list

}
