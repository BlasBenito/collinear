#' Validate Arguments \code{response} or \code{responses}
#'
#' @description Internal function validate the arguments \code{response} and \code{responses}. It checks that its value exists as a column name of \code{df},
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#' @inheritParams validate_arg_quiet
#' @param max_responses (required, integer or NULL) Maximum number of responses to consider. Default: NULL
#' @return character string: response name
#' @examples
#'
#' data(vi_smol)
#'
#' x <- validate_arg_responses(
#'   df = vi_smol,
#'   responses = "vi_numeric"
#' )
#'
#' attributes(x)$validated
#'
#' @autoglobal
#' @family argument_validation
#' @export
validate_arg_responses <- function(
  df = NULL,
  responses = NULL,
  max_responses = NULL,
  quiet = FALSE,
  function_name = NULL
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_responses()",
    function_name = function_name
  )

  #if df is NULL, stop
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #return responses if NULL
  if (is.null(responses)) {
    return(NULL)
  }

  #max_responses <-
  if (is.null(max_responses)) {
    max_responses <- Inf
  } else {
    if (!is.numeric(max_responses)) {
      stop(
        "\n",
        function_name,
        ": argument 'max_responses' must be a integer.",
        call. = FALSE
      )
    }

    max_responses <- as.integer(max_responses)
  }

  #arg name for messages
  if (max_responses == 1) {
    arg_name <- "response"
  } else {
    arg_name <- "responses"
  }

  #return responses if validated
  if (
    isTRUE(attr(x = responses, which = "validated")) &&
      all(responses %in% colnames(df)) &&
      length(responses) <= max_responses
  ) {
    return(responses)
  }

  #check wrong responses
  wrong_responses <- setdiff(
    x = responses,
    y = colnames(df)
  )

  if (length(wrong_responses) > 0L) {
    if (all(responses %in% wrong_responses)) {
      if (quiet == FALSE) {
        message(
          "\n",
          function_name,
          ": argument '",
          arg_name,
          "' does not contain column names of 'df' and will be ignored."
        )
      }

      return(NULL)
    }

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": the following values of argument '",
        arg_name,
        "' are not column names of 'df' and will be ignored:\n - ",
        paste(
          wrong_responses,
          collapse = "\n - "
        )
      )
    }
  }

  responses <- intersect(
    x = responses,
    y = colnames(df)
  )

  #check length
  if (length(responses) == 0) {
    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": argument '",
        arg_name,
        "' does not contain column names of 'df' and will be ignored."
      )
    }

    return(NULL)
  }

  if (length(responses) > max_responses) {
    responses <- responses[1:max_responses]

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": argument '",
        arg_name,
        "' must be of length ",
        max_responses,
        ", using ",
        arg_name,
        ": \n - ",
        paste(
          responses,
          collapse = "\n - "
        )
      )
    }
  }

  attr(
    x = responses,
    which = "validated"
  ) <- TRUE

  responses
}
