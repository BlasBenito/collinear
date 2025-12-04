#' Identify Response Type
#'
#' @description
#' Used by [f_auto()] to identify the type of a response variable and select a proper modelling method to compute preference order.
#' Supported types are:
#' \itemize{
#'   \item "continuous-binary": decimal numbers and two unique values; results in a warning, as this type is difficult to model.
#'   \item "continuous-low": decimal numbers and 3 to 5 unique values; results in a message, as this type is difficult to model.
#'   \item "continuous-high": decimal numbers and more than 5 unique values.
#'   \item "integer-binomial": integer with 0s and 1s, suitable for binomial models.
#'   \item "integer-binary": integer with 2 unique values other than 0 and 1; returns a warning, as this type is difficult to model.
#'   \item "integer-low": integer with 3 to 5 unique values or meets specified thresholds.
#'   \item "integer-high": integer with more than 5 unique values suitable for count modelling.
#'   \item "categorical": character or factor with 2 or more levels.
#'   \item "unknown": when the response type cannot be determined.
#' }
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#'
#' @return character string: response type
#'
#' @examples
#' data(vi_smol)
#'
#' identify_response_type(
#'   df = vi_smol,
#'   response = "vi_numeric"
#' )
#'
#' identify_response_type(
#'   df = vi_smol,
#'   response = "vi_counts"
#' )
#'
#' identify_response_type(
#'   df = vi_smol,
#'   response = "vi_binomial"
#' )
#'
#' identify_response_type(
#'   df = vi_smol,
#'   response = "vi_categorical"
#' )
#'
#' identify_response_type(
#'   df = vi_smol,
#'   response = "vi_factor"
#' )
#'
#' @export
#' @family data_types
#' @autoglobal
identify_response_type <- function(
  df = NULL,
  response = NULL,
  quiet = FALSE,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_response_type()",
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

  if (is.null(response)) {
    stop(
      "\n",
      function_name,
      ": argument 'response' must not be NULL.",
      call. = FALSE
    )
  }

  response <- validate_arg_responses(
    df = df,
    responses = response,
    max_responses = 1,
    quiet = quiet,
    function_name = function_name
  )

  df <- validate_arg_df(
    df = df,
    responses = response,
    predictors = NULL,
    quiet = quiet,
    function_name = function_name
  )

  # extract response
  x <- sort(unique(df[[response]]))
  x_length <- length(x)

  #if constant, error
  if (x_length == 1) {
    stop(
      "\n",
      function_name,
      ": Argument 'response' with value '",
      response,
      "' names a column with constant values. Please select a different response column.",
      call. = FALSE
    )
  }

  #numeric types
  if (is.numeric(x)) {
    # continuous values
    if (!all(x %% 1 == 0)) {
      if (x_length == 2) {
        if (quiet == FALSE) {
          message(
            "\n",
            function_name,
            ": argument 'response' names a numeric non-integer column with two unique values. Please consider recoding it as categorical, or select a different response column."
          )
        }

        return("continuous-binary")
      } else if (x_length <= 5) {
        if (quiet == FALSE) {
          message(
            "\n",
            function_name,
            ": argument 'response' names a numeric non-integer column with 5 or fewer values. Please consider recoding it as integer or categorical, or select a different response column."
          )
        }

        return("continuous-low")
      } else {
        return("continuous-high")
      }
    } else {
      #integer values

      #integer counts
      if (all(x == as.integer(x))) {
        if (x_length == 2) {
          # binomial: 0s and 1s
          if (all(x %in% c(0, 1))) {
            return("integer-binomial")
          } else {
            return("integer-binary")
          }
        } else if (
          x_length <= 5 ||
            max(x) <= 15 ||
            mean(x) <= 5
        ) {
          return("integer-low")
        } else {
          return("integer-high")
        }
      }
    }
  }

  # categorical
  if (is.factor(x) || is.character(x)) {
    return("categorical")
  }

  return("unknown")
}
