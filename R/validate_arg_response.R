#' Validate Argument \code{response}
#'
#' @description
#' Internal function. Validates the argument \code{response} as follows.
#' \itemize{
#'   \item Returns \code{response} as-is if it has the attribute \code{validated = TRUE}.
#'   \item Stops if \code{df} is \code{NULL}, using [validate_arg_df_not_null()].
#'   \item Returns \code{NULL} immediately if \code{response} is \code{NULL}.
#'   \item If multiple values are passed to \code{response}, uses the first and issues a warning.
#'   \item Returns \code{NULL} with a warning if \code{response} is not a column name in \code{df}.
#'   \item Returns \code{NULL} with a warning if \code{response} is numeric with near zero variance.
#'   \item Returns \code{NULL} with a warning if \code{response} is non-numeric and has constant values.
#'   \item Issues a message (if \code{quiet = FALSE}) if the response column contains \code{NA} values.
#'   \item Adds the attribute \code{validated = TRUE} to the returned \code{response}.
#' }
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#' @return character string: response name
#' @examples
#'
#' data(vi)
#'
#' response <- validate_arg_response(
#'   df = vi,
#'   response = "vi_numeric"
#' )
#'
#' attributes(response)$validated
#'
#' @autoglobal
#' @family data_validation
#' @export
validate_arg_response <- function(
    df = NULL,
    response = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(isTRUE(attr(x = response, which = "validated"))){
    return(response)
  }

  #if df is NULL, stop
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(response)){
    return(NULL)
  }

  #check that the response is in df
  if(any(!response %in% colnames(df)) == TRUE){

    missing_response <- setdiff(
      x = response,
      y = colnames(df)
    )

    warning(
      function_name,
      ": the following column/s in 'response' are not in 'df' and will be ignored: \n - ",
      paste(missing_response, collapse = "\n - "),
      call. = FALSE
    )

    response <- intersect(
      x = names(df),
      y = response
    )

  }

  #check length
  if(length(response) > 1){

    message(
      function_name,
      ": multiple columns are named in the argument 'response'. Using first one with value '", response[1], "'."
    )

    response <- response[1]

  }

  if(length(response) == 0){

    warning(
      function_name,
      ": no response available, returning NULL.",
      call. = FALSE
    )

    return(NULL)

  }

  #check that it has not near-zero variance
  if(is.numeric(df[[response]])){

    response.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = response
    )

    if(length(response.zero.variance) == 1){

      warning(
        function_name,
        ": argument 'response' with value '",
        response,
        "' refers to a column with near zero variance values and will be ignored.",
        call. = FALSE
      )

      return(NULL)

    }

  } else {

    #check that it is not constant
    if(length(unique(df[[response]])) == 1){

      warning(
        function_name,
        ": argument 'response' with value '",
        response,
        "' refers to a non-numeric column with constant values and will be ignored.",
        call. = FALSE
      )

      return(NULL)

    }

  }

  #check NA
  response.na.values <- sum(is.na(df[[response]]))

  if(response.na.values > 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' with value '",
        response,
        "' refers to a column with ",
        response.na.values,
        " NA values and may cause unexpected issues."
      )

    }

  }

  attr(
    x = response,
    which = "validated"
  ) <- TRUE

  response

}
