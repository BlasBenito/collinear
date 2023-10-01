#' Inspects argument df
#'
#' @param df (required; data frame or matrix) Input data frame
#' @param minimum_rows (required; integer) Minimum number of rows appropriate for a pairwise correlation or a variance inflation factor analysis.
#' @param minimum_numeric_columns (required, integer) Minimum number of numeric columns required for a pairwise correlation or a variance inflation factor analysis.
#'
#' @return data frame
#' @example
#' if (interactive()) {
#'   data(ecoregions)
#'   ecoregions <- df_inspect(df = ecoregions)
#' }
#' @export
#' @autoglobal
df_inspect <- function(
    df = NULL,
    minimum_rows = 30,
    minimum_numeric_columns = 1
    ){

  #handle df = NULL
  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  #handle coercion to df
  if(is.data.frame(df) == FALSE){
    df <- tryCatch(
      {as.data.frame(df)},
      error = function(e){
        stop("Argument 'df' must be a data frame or a matrix.")
      }
    )
  }

  #number of numeric columns must be > 0
  if(length(df_numeric_columns(df)) == 0){
    stop("Argument 'df' must have at least one numeric column.")
  }

  #number of rows must be > 30
  if(nrow(df) < minimum_rows){
    warning("Number of rows in argument 'df' is low. This function may fail or yield meaningless results.")
  }

  df

}

#' Checks 'predictors' argument
#'
#' @param df data argument.
#' @param predictors predictors.argument
#' @param numeric.only logical
#' @param zero.variance.allowed logical
#' @param decimal_places integer, number of decimals for the zero variance test
#' @param verbose logical
#'
#' @return predictor.names
#' @export
#' @rdname internal
#' @keywords internal
predictors_inspect <- function(
    df = NULL,
    predictors = NULL,
    numeric.only = TRUE,
    zero.variance.allowed = FALSE,
    decimal_places = 4,
    verbose = TRUE
){

  #if predictors is null, use colnames(df)
  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  #show predictors that are possibly mispelled
  #subset predictors to colnames(df)
  predictors.not.in.df <- setdiff(
    x = colnames(df),
    y = predictors
  )

  if(length(predictors.not.in.df) > 0){

    warning(
      "These predictors are not column names of argument 'df' and will be ignored: ",
      paste(predictors.not.in.df, collapse = ", ")
      )

    predictors <- intersect(
      x = predictors,
      y = colnames(df)
    )

  }


  #removing zero variance predictors

    zero.variance.columns <- df_zero_variance_columns(
      df = df,
      columns = predictors,
      decimal_places = decimal_places
    )

    if(length(zero.variance.columns) > 0){

      if(verbose == TRUE){
        message(
          "These predictors have near zero variance and will be dropped:\n",
          paste0(
            zero.variance.columns,
            collapse = "\n"
          )
        )
      }

      predictors <- setdiff(
        predictors,
        zero.variance.columns
      )

    }


  predictors

}

#' Checks 'response' argument
#'
#' @param df df argument.
#' @param response response
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in df or not.
#' @param zero.variance.allowed logical
#' @param decimal_places integer, number of decimals for the zero variance test
#'
#' @return response
#' @export
#' @rdname internal
#' @keywords internal
check_response_name <- function(
    response = NULL,
    df = NULL,
    is.required = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    decimal_places = 4,
    verbose = TRUE
){

  if(is.null(response) == TRUE){
    if(is.required == TRUE){
      stop("Argument 'response' is required.")
    }
  }

  if(is.character(response) == FALSE){
    stop("Argument 'response' must be a character vector.")
  }

  if(length(response) != 1){
    if(is.required == TRUE){
      stop("Argument 'response' must be of length 1 but it is empty.")
    }
  }

  #check that all predictors are in df
  if(!(response %in% colnames(df))){
    if(is.required == TRUE){
      stop("Argument 'response' must be a column name of 'df'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response' must be a column name of 'df'.")
      }
    }
  }

  if(is.numeric(df[[response]]) == FALSE){
    if(is.required == TRUE){
      stop("Argument 'response' must be a numeric column of 'df'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response' is not a numeric column of 'df' and will be ignored.")
      }
      return(NULL)
    }

  } else {

    if(zero.variance.allowed == FALSE){
      if(var(round(df[[response]], decimal_places)) == 0){
        if(is.required == TRUE){
          stop("Argument 'response' is the name of a column with near zero variance.")
        } else {
          if(verbose == TRUE){
            message("Argument 'response' is the name of a column with near zero variance. This might cause numerical issues.")
          }
        }
      }
    }

  }

  if(na.allowed == FALSE){

    if(sum(is.na(df[[response]])) > 0){
      if(is.required == TRUE){
        stop("Argument 'response' is the name of a column with NA values.")
      } else {
        if(verbose == TRUE){
          message("Argument 'response' is the name of a column with NA values. This might cause unintended issues.")
        }
      }
    }

  }

  response

}


#' Extract Numeric Columns from a Data Frame
#'
#' This function takes a df frame and a set of columns and returns the names of the numeric columns in the selected data frame.
#'
#' @param df A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the numeric columns in the selected df frame.
#' @export
#' @rdname internal
#' @keywords internal
df_non_numeric_columns <- function(
    df,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(df)
  }

  df <- df[, columns]

  out <- colnames(df)[!sapply(df, is.numeric)]

  out

}


#' Extract zero-variance Columns from a df Frame
#'
#' This function takes a df frame and a set of columns and returns the names of the columns with zero variance.
#'
#' @param df A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @param decimal_places Integer, number of decimal places to round `columns` to. Defines the tolerance of the test. Default: 4
#' @return A character vector with the names of the numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
df_zero_variance_columns <- function(
    df,
    columns = NULL,
    decimal_places = 4
){

  if(is.null(columns)){
    columns <- colnames(df)
  }

  numeric.columns <- df_numeric_columns(
    df = df,
    columns = columns
  )

  df <- df[, numeric.columns]

  zero.variance.columns <- colnames(df)[
    round(
      apply(
        X = df,
        MARGIN = 2,
        FUN = var
      ),
      4
    ) == 0
  ]

  zero.variance.columns

}


#' @title Rescales a numeric vector into a new range
#' @description Rescales a numeric vector to a new range.
#' @param x (required, numeric vector) Numeric vector. Default: `NULL`
#' @param new_min (optional, numeric) New minimum value. Default: `0`
#' @param new_max (optional_numeric) New maximum value. Default: `1`
#' @param old_min (optional, numeric) Old minimum value. Default: `NULL`
#' @param old_max (optional_numeric) Old maximum value. Default: `NULL`
#' @return A numeric vector of the same length as x, but with its values rescaled between `new_min` and `new_max.`
#' @examples
#' if(interactive()){
#'
#'  out <- rescale_vector(
#'    x = rnorm(100),
#'    new_min = 0,
#'    new_max = 100,
#'    integer = TRUE
#'    )
#'    out
#'
#' }
#' @rdname rescale_vector
#' @export
rescale_vector <- function(x = NULL,
                           new_min = 0,
                           new_max = 1,
                           old_min = NULL,
                           old_max = NULL){

  if(is.null(x) | !is.vector(x) | !is.numeric(x)){
    stop("x must be a numeric vector.")
  }

  #data extremes
  if(is.null(old_min)){
    old_min = min(x)
  }

  if(is.null(old_max)){
    old_max = max(x)
  }

  #scaling
  x = ((x - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min

  x

}

#' Extract Non-numeric Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the non-numeric columns in the selected data frame.
#'
#' @param df A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the non-numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
df_numeric_columns <- function(
    df = NULL,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(df)
  }

  df <- df[, columns]

  out <- colnames(df)[sapply(df, is.numeric)]

  out

}

