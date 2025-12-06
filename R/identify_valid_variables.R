#' Find valid numeric, categorical, and logical variables in a dataframe
#'
#' @description
#' Returns a list with the names of the valid numeric, categorical, and logical variables in a modelling dataframe.
#'
#' @inheritParams collinear
#' @inheritParams identify_zero_variance_variables
#' @param predictors (required, character vector) Names of the predictors to identify. Default: NULL
#' @return list
#' \itemize{
#'   \item \code{numeric}: character vector of numeric predictors.
#'   \item \code{categorical}: character vector of categorical (character and factor) predictors.
#'   \item \code{logical}: character vector of logical predictors.
#' }
#' @examples
#'
#' data(vi_smol, vi_predictors)
#'
#' x <- identify_valid_variables(
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
identify_valid_variables <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  decimals = 4,
  quiet = FALSE,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_valid_variables()",
    ... = ...
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
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )
  }

  variables_numeric <- identify_numeric_variables(
    df = df,
    responses = responses,
    predictors = predictors,
    decimals = decimals,
    quiet = quiet,
    function_name = function_name
  )$valid

  variables_categorical <- identify_categorical_variables(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )$valid

  variables_logical <- identify_logical_variables(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )$valid

  out_list <- list()

  if (!is.null(variables_numeric)) {
    out_list$numeric <- variables_numeric
  }

  if (!is.null(variables_categorical)) {
    out_list$categorical <- variables_categorical
  }

  if (!is.null(variables_logical)) {
    out_list$logical <- variables_logical
  }

  out_list
}
