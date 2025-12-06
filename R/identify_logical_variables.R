#' Find logical variables in a dataframe
#'
#' @description
#' Identifies logical predictors and excludes those with constant values.
#'
#' @inheritParams identify_valid_variables
#' @return list:
#' \itemize{
#'   \item \code{valid}: character vector with valid logical predictor names.
#'   \item \code{invalid}: character vector with invalid logical predictor names.
#' }
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' #invalid logical
#' vi_smol$logical_invalid <- TRUE
#'
#' #valid logical
#' vi_smol$logical_valid <- sample(
#'   x = c(TRUE, FALSE),
#'   size = nrow(vi_smol),
#'   replace = TRUE
#' )
#'
#' x <- identify_logical_variables(
#'   df = vi_smol,
#'   predictors = c(
#'     vi_predictors,
#'     "logical_invalid",
#'     "logical_valid"
#'   )
#' )
#'
#' x$valid
#' x$invalid
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_logical_variables <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  quiet = FALSE,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_logical_variables()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
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

  # Get logical predictors
  predictors_logical_all <- predictors[
    vapply(
      X = df[, predictors, drop = FALSE],
      FUN = is.logical,
      FUN.VALUE = logical(1)
    )
  ] |>
    stats::na.omit()

  if (length(predictors_logical_all) == 0) {
    return(out_list)
  }

  #keep predictors with unique length different than one or nrow(df)
  length_unique <- vapply(
    X = df[, predictors_logical_all, drop = FALSE],
    FUN = function(x) {
      length(unique(x))
    },
    FUN.VALUE = integer(1)
  )

  predictors_logical_valid <- predictors_logical_all[length_unique > 1]

  predictors_logical_invalid <- setdiff(
    x = predictors_logical_all,
    y = predictors_logical_valid
  )

  if (
    quiet == FALSE &&
      length(predictors_logical_invalid) > 0
  ) {
    message(
      "\n",
      function_name,
      ": invalid logical ",
      vars_string,
      " due to constant values:\n - ",
      paste(
        predictors_logical_invalid,
        collapse = "\n - "
      )
    )
  }

  if (length(predictors_logical_valid) > 0) {
    out_list$valid <- predictors_logical_valid
  }

  if (length(predictors_logical_invalid) > 0) {
    out_list$invalid <- predictors_logical_invalid
  }

  out_list
}
