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
#' @author Blas M. Benito, PhD
#' @export
identify_numeric_predictors <- function(
    df = NULL,
    predictors = NULL
){

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

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

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

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

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

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
