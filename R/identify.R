#' Identify numeric predictors
#'
#' Given validated 'df' and 'predictors' arguments, this function subsets and returns the numeric predictors.
#'
#' @param df (required; data frame or tibble) A validated data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param predictors (optional; character vector) A validated vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @return character vector with names of numeric predictors.
#' @examples
#' if (interactive()) {
#'   #load example data
#'   #and its predictors
#'   data(
#'     vi,
#'     vi_predictors
#'     )
#'
#'   #validate the vi data frame
#'   vi <- collinear::validate_df(df = vi)
#'
#'   #validate predictors
#'   predictors <- collinear::validate_df_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#'   )
#'
#'   #get numeric predictors
#'   predictors.numeric <- collinear::identify_numeric_predictors(
#'     df = vi,
#'     predictors = predictors
#'     )
#' }
#' @autoglobal
#' @export
identify_numeric_predictors <- function(
    df = NULL,
    predictors = NULL
){

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #if already validated, return it
  if(is.null(attr(df, "validated"))){
    stop("argument 'df' is not validated. Please, run validate_df() before identify_numeric_predictors().")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  } else {
    #stop if not validated
    if(is.null(attr(predictors, "validated"))){
      stop("argument 'predictors' is not validated. Please run validate_predictors().")
    }
  }

  df <- df[, predictors]

  predictors.numeric <- colnames(df)[sapply(df, is.numeric)]

  attr(
    x = predictors.numeric,
    which = "validated"
  ) <- TRUE

  predictors.numeric

}

#' Identify non-numeric predictors
#'
#' Given validated 'df' and 'predictors' arguments, this function subsets and returns the non-numeric (character, factor, and logical) predictors.
#'
#' @param df (required; data frame or tibble) A validated data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param predictors (optional; character vector) A validated vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @return character vector with names of numeric predictors.
#' @examples
#' if (interactive()) {
#'   #load example data
#'   #and its predictors
#'   data(
#'     vi,
#'     vi_predictors
#'     )
#'
#'   #validate the vi data frame
#'   vi <- collinear::validate_df(df = vi)
#'
#'   #validate predictors
#'   predictors <- collinear::validate_df_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#'   )
#'
#'   #get numeric predictors
#'   predictors.numeric <- collinear::identify_non_numeric_predictors(
#'     df = vi,
#'     predictors = predictors
#'     )
#' }
#' @autoglobal
#' @export
identify_non_numeric_predictors <- function(
    df = NULL,
    predictors = NULL
){

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #if already validated, return it
  if(is.null(attr(df, "validated"))){
    stop("argument 'df' is not validated. Please, run validate_df() before identify_non_numeric_predictors().")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  } else {
    #stop if not validated
    if(is.null(attr(predictors, "validated"))){
      stop("argument 'predictors' is not validated. Please run validate_predictors().")
    }
  }

  df <- df[, predictors]

  predictors.non.numeric <- colnames(df)[!sapply(df, is.numeric)]

  attr(
    x = predictors.non.numeric,
    which = "validated"
  ) <- TRUE

  predictors.non.numeric

}


#' Identify zero and near-zero-variance predictors
#'
#' Predictors a variance of zero or near zero are highly problematic for multicollinearity analysis and modelling in general. This function identifies these predictors with a level of sensitivity defined by the 'decimals' argument. Smaller number of decimals increase the number of variables detected as near zero variance. Recommended values will depend on the range of the numeric variables in 'df'.
#'
#' @param df (required; data frame or tibble) A validated data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param predictors (optional; character vector) A validated vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param decimals (required, integer) number of decimal places for the zero variance test. Default: 4
#' @return character vector with names of zero and near-zero variance columns.
#' @examples
#' if (interactive()) {
#'   #load example data
#'   #and its predictors
#'   data(
#'     vi,
#'     vi_predictors
#'     )
#'
#'   #validate the vi data frame
#'   vi <- collinear::validate_df(df = vi)
#'
#'   #validate predictors
#'   predictors <- collinear::validate_df_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#'   )
#'
#'   #get numeric predictors
#'   predictors.numeric <- collinear::identify_non_numeric_predictors(
#'     df = vi,
#'     predictors = predictors
#'     )
#' }
#' @autoglobal
#' @export
identify_zero_variance_predictors <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4
){

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #if already validated, return it
  if(is.null(attr(df, "validated"))){
    stop("argument 'df' is not validated. Please, run validate_df() before identify_zero_variance_predictors().")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  } else {
    #stop if not validated
    if(is.null(attr(predictors, "validated"))){
      stop("argument 'predictors' is not validated. Please run validate_predictors().")
    }
  }

  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  df <- df[, predictors.numeric]

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

  attr(
    x = zero.variance.predictors,
    which = "validated"
  ) <- TRUE

  zero.variance.predictors

}

