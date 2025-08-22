#' Validate Argument \code{predictors} for Correlation Analysis
#'
#' @description
#' Internal function to assess whether the input arguments \code{df} and \code{predictors} result in data dimensions suitable for pairwise correlation analysis. Expects argument \code{df} to be validated with [validate_arg_df()].
#'
#' The function performs the following actions:
#' \itemize{
#'   \item Returns \code{predictors} immediately if it has the attribute \code{validated_cor = TRUE}.
#'   \item If \code{predictors} does not have the attribute \code{validated = TRUE}, it is validated with [validate_arg_predictors()].
#'   \item Stops if \code{df} is \code{NULL}, using [validate_arg_df_not_null()].
#'   \item If \code{predictors} is empty, issues a warning and returns \code{NULL}.
#'   \item If only one predictor is provided, returns it with a message and skips pairwise correlation filtering.
#'   \item If fewer than 10 complete cases are available for the selected predictors, issues a warning and returns \code{NULL}.
#'   \item Adds the attributes \code{validated = TRUE} and \code{validated_cor = TRUE} to the returned predictor vector.
#' }
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
#' @examples
#' data(vi, vi_predictors)
#'
#' predictors <- validate_arg_predictors_cor(
#'   df = vi,
#'   predictors = vi_predictors,
#'   function_name = "cor_df()"
#' )
#'
#'   attributes(predictors)$validated
#'   attributes(predictors)$validated_cor
validate_arg_predictors_cor <- function(
    df = NULL,
    predictors = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(isTRUE(attr(x = predictors, which = "validated_cor"))){
    return(predictors)
  }

  if(!isTRUE(attr(x = predictors, which = "validated"))){

    predictors <- validate_arg_predictors(
      df = df,
      response = NULL,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet
    )

  }

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(length(predictors) == 0){

      message(
        "\n",
        function_name,
        ": no predictors available, skipping correlation filtering."
      )

    return(NULL)

  }

  if(length(predictors) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one predictor in argument 'predictors', skipping correlation filtering."
      )

    }

    return(predictors)

  }

  attr(
    x = predictors,
    which = "validated"
  ) <- TRUE

  attr(
    x = predictors,
    which = "validated_cor"
  ) <- TRUE

  predictors

}
