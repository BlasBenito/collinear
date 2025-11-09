#' Identify Near-Zero Variance Variables
#'
#'
#' @description
#' Returns the names of near-zero variance variables in a modelling dataframe.
#'
#' @inheritParams collinear
#' @param decimals (required, integer) Number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4
#' @return character vector: names of near-zero variance columns.
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' #create zero and near variance predictors
#' vi_smol$zero_variance <- 1
#' vi_smol$near_zero_variance <- runif(
#'   n = nrow(vi_smol),
#'   min = 0,
#'   max = 0.0001
#'   )
#'
#'
#' #add to vi predictors
#' vi_predictors <- c(
#'   vi_predictors,
#'   "zero_variance",
#'   "near_zero_variance"
#' )
#'
#' #identify zero variance predictors
#' x <- identify_zero_variance_variables(
#'   df = vi_smol,
#'   predictors = vi_predictors
#' )
#'
#' x
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_zero_variance_variables <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    decimals = 4,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_zero_variance_variables()",
    ... = ...
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(!is.null(responses)){

    responses <- validate_arg_responses(
      df = df,
      responses = responses,
      quiet = quiet,
      function_name = function_name
    )

  }

  if(!is.null(predictors)){

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  vars_string <- if (!is.null(predictors) && !is.null(responses)) {
    "variables"
  } else if (!is.null(predictors)) {
    "predictors"
  } else if (!is.null(responses)) {
    "responses"
  } else {
    "variables"
  }

  predictors <- c(responses, predictors)

  if(is.null(predictors) || length(predictors) == 0){

    stop(
      "\n",
      function_name,
      ": there are no ",
      vars_string,
      " to identify.",
      call. = FALSE
    )

  }

  decimals <- as.integer(decimals)

  df <- df[, predictors, drop = FALSE]

  predictors <- predictors[
    vapply(
      X = df,
      FUN = is.numeric,
      FUN.VALUE = logical(1)
    )
  ]

  if(length(predictors) == 0){
    return(NULL)
  }

  df <- df[, predictors, drop = FALSE]

  #compute variance on valid values only
  predictors <- colnames(df)[
    round(
      sapply(
        X = df,
        FUN = function(x) stats::var(
          x = x[is.finite(x)],
          na.rm = TRUE
        )
      ),
      decimals
    ) == 0
  ]

  if(
    quiet == FALSE &&
    length(predictors) > 0
  ){

    message(
      "\n",
      function_name,
      ": invalid ",
      vars_string,
      " due to near-zero variance:\n - ",
      paste(
        predictors,
        collapse = "\n - "
      )
    )

  }

  if(length(predictors) == 0){
    predictors <- NULL
  }

  predictors

}
