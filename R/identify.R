#' Identify Numeric and Categorical Predictors
#'
#' @description
#' Returns a list with the names of the valid numeric predictors and the names of the valid categorical predictors
#'
#' @inheritParams collinear
#' @inheritParams identify_predictors_zero_variance
#' @return list: names of numeric, categorical, logical, and zero-variance predictors
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' predictors_names <- identify_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' predictors_names
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_predictors <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors()",
    ... = ...
  )

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  predictors_numeric <- identify_predictors_numeric(
    df = df,
    predictors = predictors,
    decimals = decimals,
    quiet = quiet,
    function_name = function_name
  )

  predictors_categorical <- identify_predictors_categorical(
    df = df,
    predictors = setdiff(
      x = predictors,
      y = predictors_numeric
    ),
    quiet = quiet,
    function_name = function_name
  )

  predictors_logical <- identify_predictors_logical(
    df = df,
    predictors = setdiff(
      x = predictors,
      y = c(
        predictors_numeric,
        predictors_categorical
        )
    ),
    decimals = decimals,
    quiet = quiet,
    function_name = function_name
  )

  predictors_zero_variance <- identify_predictors_zero_variance(
    df = df,
    predictors = setdiff(
      x = predictors,
      y = c(
        predictors_numeric,
        predictors_categorical,
        predictors_logical
        )
    ),
    decimals = decimals,
    quiet = quiet,
    function_name = function_name
  )

  out_list <- list(
    numeric = predictors_numeric,
    categorical = predictors_categorical,
    logical = predictors_logical,
    zero_variance = predictors_zero_variance
  )

  out_list

}

#' Identify Valid Logical Predictors
#'
#' @description
#' Returns the names of valid logical predictors. Ignores predictors with constant values (i.e., all TRUE or all FALSE).
#'
#' @inheritParams collinear
#' @inheritParams identify_predictors_zero_variance
#' @return character vector: names of logical predictors
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' logical.predictors <- identify_predictors_logical(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' logical.predictors
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_predictors_logical <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors_logical()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors) || length(predictors) == 0){
    return(NULL)
  }

  predictors <- intersect(
    x = predictors,
    y = colnames(df)
  )

  df <- df[, predictors, drop = FALSE]

  # Get logical predictors
  predictors <- predictors[
    vapply(
      X = df,
      FUN = is.logical,
      FUN.VALUE = logical(1)
    )
  ]

  if(length(predictors) == 0){
    return(NULL)
  }

  #ignore constant predictors (TRUE/FALSE only)
  predictors_constant <- identify_predictors_zero_variance(
    df = df,
    predictors = predictors,
    decimals = decimals,
    quiet = quiet
  )

  if(quiet == FALSE && length(predictors_constant) < 0){

    message(
      "\n",
      function_name,
      ": these logical predictors have constant values and will be ignored:\n - ",
      paste(
        predictors_constant,
        collapse = "\n - "
      )
    )

  }

  predictors <- setdiff(
    x = predictors,
    y = predictors_constant
  )

  if(length(predictors) == 0){
    predictors <- NULL
  }

  predictors

}


#' Identify Valid Numeric Predictors
#'
#' @description
#' Returns the names of valid numeric predictors. Ignores predictors with constant values or with near-zero variance.
#'
#' @inheritParams collinear
#' @inheritParams identify_predictors_zero_variance
#' @return character vector: names of numeric predictors
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' numeric.predictors <- identify_predictors_numeric(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' numeric.predictors
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_predictors_numeric <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors_numeric()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors) || length(predictors) == 0){
    return(NULL)
  }

  predictors <- intersect(
    x = predictors,
    y = colnames(df)
  )

  df <- df[, predictors, drop = FALSE]

  #get numeric predictors
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

  #ignore constant and near-zero variance predictors
  predictors_zero_variance <- identify_predictors_zero_variance(
    df = df,
    predictors = predictors,
    function_name = function_name
  )

  if(quiet == FALSE && length(predictors_zero_variance) < 0){

    message(
      "\n",
      function_name,
      ": these numeric predictors have constant values and will be ignored:\n - ",
      paste(
        predictors_zero_variance,
        collapse = "\n - "
      )
    )

  }

  predictors <- setdiff(
    x = predictors,
    y = predictors_zero_variance
  )

  if(length(predictors) == 0){
    predictors <- NULL
  }

  predictors

}

#' Identify Valid Categorical Predictors
#'
#' @description
#' Returns the names of character or factor predictors, if any. Removes categorical predictors with constant values, or with as many unique values as rows are in the input data frame.
#'
#'
#' @inheritParams collinear
#' @return character vector: categorical predictors names
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' non.numeric.predictors <- identify_predictors_categorical(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' non.numeric.predictors
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_predictors_categorical <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors_categorical()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors) || length(predictors) == 0){
    return(NULL)
  }

  df <- df[, predictors, drop = FALSE]

  #subset categorical
  predictors <- predictors[
    !vapply(
      X = df,
      FUN = is.numeric,
      FUN.VALUE = logical(1)
    )
  ]

  #remove NA
  predictors <- stats::na.omit(predictors)

  predictors_copy <- predictors

  #remove constant categoricals
  predictors <- predictors[
    !vapply(
      X = df[, predictors, drop = FALSE],
      FUN = function(x){
        length(unique(x)) == 1
      },
      FUN.VALUE = logical(1)
    )
  ]

  if(quiet == FALSE && length(predictors) < length(predictors_copy)){

    predictors_constant <- setdiff(
      x = predictors_copy,
      y = predictors
    )

    message(
      "\n",
      function_name,
      ": these categorical predictors have constant values and will be ignored:\n - ",
      paste(
        predictors_constant,
        collapse = "\n - "
      )
    )

  }

  #remove categoricals with as many values as rows
  predictors_copy <- predictors

  predictors <- predictors[
    !vapply(
      X = df[, predictors, drop = FALSE],
      FUN = function(x) length(unique(x)) == length(x),
      FUN.VALUE = logical(1)
    )
  ]

  if(quiet == FALSE && length(predictors) < length(predictors_copy)){

    predictors_missing <- setdiff(
      x = predictors_copy,
      y = predictors
    )

    message(
      "\n",
      function_name,
      ": these categorical predictors have as many unique values as rows and will be ignored:\n - ",
      paste(
        predictors_missing,
        collapse = "\n - "
      )
    )

  }

  if(length(predictors) == 0){
    predictors <- NULL
  }

  predictors

}


#' Identify Zero and Near-Zero Variance Predictors
#'
#'
#' @description
#' Variables with a variance of zero or near-zero are highly problematic for multicollinearity analysis and modelling in general. This function identifies these variables with a level of sensitivity defined by the 'decimals' argument. Smaller number of decimals increase the number of variables detected as near zero variance. Recommended values will depend on the range of the numeric variables in 'df'.
#'
#' @inheritParams collinear
#' @param decimals (required, integer) Number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4
#' @return character vector: names of zero and near-zero variance columns.
#' @examples
#'
#' data(vi, vi_predictors)
#'
#' #create zero variance predictors
#' vi$zv_1 <- 1
#' vi$zv_2 <- runif(n = nrow(vi), min = 0, max = 0.0001)
#'
#'
#' #add to vi predictors
#' vi_predictors <- c(
#'   vi_predictors,
#'   "zv_1",
#'   "zv_2"
#' )
#'
#' #identify zero variance predictors
#' zero.variance.predictors <- identify_predictors_zero_variance(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' zero.variance.predictors
#'
#' @autoglobal
#' @family data_types
#' @author Blas M. Benito, PhD
#' @export
identify_predictors_zero_variance <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors_zero_variance()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors) || length(predictors) == 0){
    return(NULL)
  }

  predictors <- intersect(
    x = predictors,
    y = colnames(df)
  )

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

  if(length(predictors) == 0){
    predictors <- NULL
  }

  predictors

}

#' Identify Response Type
#'
#' @description
#' Internal function to identify the type of response variable. Supported types are:
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
#' data(vi, vi_predictors)
#'
#' identify_response_type(
#'   df = vi,
#'   response = "vi_numeric"
#' )
#'
#' identify_response_type(
#'   df = vi,
#'   response = "vi_counts"
#' )
#'
#' identify_response_type(
#'   df = vi,
#'   response = "vi_binomial"
#' )
#'
#' identify_response_type(
#'   df = vi,
#'   response = "vi_categorical"
#' )
#'
#' identify_response_type(
#'   df = vi,
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
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_response_type()",
    ... = ...
  )

  df <- validate_arg_df(
    df = df,
    function_name = function_name
  )

  response <- validate_arg_response(
    df = df,
    response = response,
    function_name = function_name
  )

  if(is.null(response)) {
    return(NULL)
  }

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  # extract response
  x <- sort(unique(df[[response]]))
  x_length <- length(x)

  #if constant, error
  if(x_length == 1){

    stop(
      "\n",
      function_name,
      ": argument 'response' names a column with a constant value. Please select a different response column.",
      call. = FALSE
    )

  }

  #numeric types
  if(is.numeric(x)) {

    # continuous values
    if(!all(x %% 1 == 0)) {

      if (x_length == 2) {

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": argument 'response' names a numeric non-integer column with two unique values. Please consider recoding it as categorical, or select a different response column."
          )

        }

        return("continuous-binary")

      } else if (x_length <= 5) {

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": argument 'response' names a numeric non-integer column with 5 or fewer values. Please consider recoding it as integer or categorical, or select a different response column.",
          )

        }

        return("continuous-low")

      } else {

        return("continuous-high")

      }

    } else {
      #integer values

      #integer counts
      if(all(x == as.integer(x))){

        if(x_length == 2){

          # binomial: 0s and 1s
          if(all(x %in% c(0, 1))){

            return("integer-binomial")

          } else {

            if(quiet == FALSE){

              message(
                "\n",
                function_name,

                ": argument 'response' names a integer column with two unique values that are not 0 and 1. Please consider recoding it as categorical, or select a different response column."
              )

            }

            return("integer-binary")

          }

        } else if(
          x_length <= 5 ||
          max(x) <= 15 ||
          mean(x) <= 5
        ){

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

#' Identify Predictor Types
#'
#' @description
#' Internal function to identify predictor types. The supported types are:
#' \itemize{
#'   \item "numeric": all predictors belong to the classes "numeric" and/or "integer".
#'   \item "categorical": all predictors belong to the classes "character" and/or "factor".
#'   \item "mixed": predictors are of types "numeric" and "categorical".
#'   \item "unknown": predictors of unknown type.
#' }
#'
#'
#' @inheritParams collinear
#'
#' @return character string: predictors type
#' @export
#' @family data_types
#' @autoglobal
#' @examples
#' data(vi, vi_predictors)
#'
#' identify_predictors_type(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' identify_predictors_type(
#'   df = vi,
#'   predictors = vi_predictors_numeric
#' )
#'
#' identify_predictors_type(
#'   df = vi,
#'   predictors = vi_predictors_categorical
#' )
#'
identify_predictors_type <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::identify_predictors_numeric()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors)){
    return(NULL)
  }

  predictors <- intersect(
    x = predictors,
    y = colnames(df)
  )

  if(length(predictors) == 0){
    return(NULL)
  }

  df <- df[, predictors, drop = FALSE]

  out <- lapply(
    X = df,
    FUN = class
  ) |>
    unlist() |>
    gsub(
      pattern = "integer",
      replacement = "numeric"
    ) |>
    gsub(
      pattern = "character|factor",
      replacement = "categorical"
    ) |>
    unique()

  if(length(out) > 1){
    return("mixed")
  }

  if(!(out %in% c("numeric", "categorical"))){
    return("unknown")
  }

  out

}
