#' Identify Numeric Variables
#'
#' @description
#' Identifies valid numeric variables and ignores those with constant values.
#'
#' @inheritParams identify_valid_variables
#' @return list:
#' \itemize{
#'   \item \code{valid}: character vector with valid numeric predictor names.
#'   \item \code{invalid}: character vector with invalid numeric predictor names due to near-zero variance.
#' }
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' x <- identify_numeric_variables(
#'   df = vi_smol,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors
#' )
#'
#' #valid numeric predictors
#' x$valid
#'
#' #invalid due to zero variance (none here)
#' x$invalid
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_numeric_variables <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  decimals = 4,
  quiet = FALSE,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_numeric_variables()",
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

  if (!is.null(responses)) {
    responses <- validate_arg_responses(
      df = df,
      responses = responses,
      quiet = quiet,
      function_name = function_name
    )
  }

  if (!is.null(predictors)) {
    predictors <- validate_arg_predictors(
      df = df,
      responses = responses,
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

  if (is.null(predictors) || length(predictors) == 0) {
    stop(
      "\n",
      function_name,
      ": there are no ",
      vars_string,
      " to identify.",
      call. = FALSE
    )
  }

  out_list <- list(
    valid = NULL,
    invalid = NULL
  )

  #get numeric predictors
  predictors_numeric_all <- predictors[
    vapply(
      X = df[, predictors, drop = FALSE],
      FUN = is.numeric,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(predictors_numeric_all) == 0) {
    return(out_list)
  }

  #ignore constant and near-zero variance predictors
  predictors_numeric_invalid <- identify_zero_variance_variables(
    df = df,
    predictors = predictors_numeric_all,
    quiet = TRUE,
    function_name = function_name
  )

  if (quiet == FALSE && length(predictors_numeric_invalid) > 0) {
    message(
      "\n",
      function_name,
      ": invalid numeric ",
      vars_string,
      " due to near-zero variance:\n - ",
      paste(
        predictors_numeric_invalid,
        collapse = "\n - "
      )
    )
  }

  predictors_numeric_valid <- setdiff(
    x = predictors_numeric_all,
    y = predictors_numeric_invalid
  )

  if (length(predictors_numeric_valid) > 0) {
    out_list$valid <- predictors_numeric_valid
  }

  if (length(predictors_numeric_invalid) > 0) {
    out_list$invalid <- predictors_numeric_invalid
  }

  out_list
}
