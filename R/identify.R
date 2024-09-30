#' Identify Numeric Variables
#'
#' @description
#' Returns the names of the numeric variables in a data frame.
#'
#' @inheritParams collinear
#' @return character vector: names of numeric predictors
#' @examples
#' if (interactive()) {
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' numeric.predictors <- identify_numeric_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' numeric.predictors
#'
#' }
#' @autoglobal
#' @family data_preparation
#' @author Blas M. Benito, PhD
#' @export
identify_numeric_predictors <- function(
    df = NULL,
    predictors = NULL
){

  df <- df[, predictors, drop = FALSE]

  predictors.numeric <- colnames(df)[sapply(df, is.numeric)]

  predictors.numeric

}

#' Identify Non-Numeric Predictors
#'
#' @description
#' Returns the names of the non-numeric predictors, if any, in a data frame.
#'
#'
#' @inheritParams collinear
#' @return character vector: names of non-numeric predictors
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' non.numeric.predictors <- identify_non_numeric_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' non.numeric.predictors
#'
#' @autoglobal
#' @family data_preparation
#' @author Blas M. Benito, PhD
#' @export
identify_non_numeric_predictors <- function(
    df = NULL,
    predictors = NULL
){

  df <- df[, predictors, drop = FALSE]

  predictors.non.numeric <- colnames(df)[!sapply(df, is.numeric)]

  predictors.non.numeric

}


#' Identify Zero and Near-Zero Variance Predictors
#'
#'
#' @description
#' Variables with a variance of zero or near-zero are highly problematic for multicollinearity analysis and modelling in general. This function identifies these variables with a level of sensitivity defined by the 'decimals' argument. Smaller number of decimals increase the number of variables detected as near zero variance. Recommended values will depend on the range of the numeric variables in 'df'.
#'
#' @inheritParams collinear
#' @param decimals (required, integer) number of decimal places for the zero variance test. Default: 4
#' @return character vector: names of zero and near-zero variance columns.
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
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
#' zero.variance.predictors <- identify_zero_variance_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' zero.variance.predictors
#'
#' @autoglobal
#' @family data_preparation
#' @author Blas M. Benito, PhD
#' @export
identify_zero_variance_predictors <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4
){

  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  df <- df[, predictors.numeric, drop = FALSE]

  #replace inf with NA
  n_inf <- lapply(
    X = df,
    FUN = is.infinite
  ) |>
    unlist() |>
    sum()

  if(n_inf > 0){
    is.na(df) <- do.call(
      what = cbind,
      args = lapply(
        X = df,
        FUN = is.infinite
      )
    )
  }

  zero.variance.predictors <- colnames(df)[
    round(
      apply(
        X = df,
        MARGIN = 2,
        FUN = var,
        na.rm = TRUE
      ),
      decimals
    ) == 0
  ]

  zero.variance.predictors

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
#'
#' @return character string: respnose type
#'
#' @examples

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
#'   response = "vi_category"
#' )
#'
#' identify_response_type(
#'   df = vi,
#'   response = "vi_factor"
#' )
#'
#' @export
#' @autoglobal
identify_response_type <- function(
    df = NULL,
    response = NULL
){

  if(is.null(df) || is.null(response)) {
    return(NULL)
  }

  # extract response
  x <- sort(unique(df[[response]]))
  x_length <- length(x)

  #if constant, error
  if(x_length == 1){

    stop(
      "collinear::identify_response_type(): argument 'response' names a column with a constant value. Please select a different response column.",
      call. = FALSE
    )

  }

  #numeric types
  if(is.numeric(x)) {

    # continuous values
    if(!all(x %% 1 == 0)) {

      if (x_length == 2) {

        warning(
          "collinear::identify_response_type(): argument 'response' names a numeric non-integer column with two unique values. Please consider recoding it as categorical, or select a different response column.",
          call. = FALSE
        )

        return("continuous-binary")

      } else if (x_length <= 5) {

        message(
          "collinear::identify_response_type(): argument 'response' names a numeric non-integer column with 5 or fewer values. Please consider recoding it as integer or categorical, or select a different response column.",
        )

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

            warning(
              "collinear::identify_response_type(): argument 'response' names a integer column with two unique values that are not 0 and 1. Please consider recoding it as categorical, or select a different response column.",
              call. = FALSE
            )

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
#' @autoglobal
#' @examples
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
    predictors = NULL
    ){

  if(is.null(df) || is.null(predictors)) {
    return(NULL)
  }

  out <- lapply(
    X = df[, predictors],
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
