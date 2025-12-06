#' Check and validate argument `predictors`
#'
#' @description
#' Validates the argument \code{predictors} by ensuring that all provided predictors are in \code{df} and don't intersect with \code{responses}, if any.
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return character vector: predictor names
#' @examples
#' data(vi_smol, vi_predictors)
#'
#' x <- validate_arg_predictors(
#'   df = vi_smol,
#'   predictors = vi_predictors
#' )
#'
#' attributes(x)$validated
#'
#' @autoglobal
#' @family argument_validation
#' @export
validate_arg_predictors <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  quiet = FALSE,
  function_name = NULL
) {
  #if already validated, return it
  if (
    isTRUE(attr(x = predictors, which = "validated")) &&
      all(predictors %in% colnames(df))
  ) {
    return(predictors)
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_predictors()",
    function_name = function_name
  )

  #if df is NULL, stop
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #if predictors is NULL, use colnames(df)
  if (is.null(predictors)) {
    predictors <- setdiff(
      x = colnames(df),
      y = responses
    )
  }

  #remove response from predictors
  predictors <- setdiff(
    x = predictors,
    y = responses
  )

  #identify wrongly named predictors
  predictors.missing <- setdiff(
    x = predictors,
    y = colnames(df)
  )

  if (length(predictors.missing) > 0) {
    if (length(predictors.missing) == length(predictors)) {
      warning(
        "\n",
        function_name,
        ": none of the 'predictors' are column names of 'df'.",
        call. = FALSE
      )

      return(NULL)
    }

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": these 'predictors' are not column names of 'df' and will be ignored:\n - ",
        paste(
          predictors.missing,
          collapse = "\n - "
        )
      )
    }
  }

  #getting predictors in df only
  predictors <- intersect(
    x = predictors,
    y = colnames(df)
  )

  attr(
    x = predictors,
    which = "validated"
  ) <- TRUE

  predictors
}
