#' Checks 'data' argument
#'
#' @param data data argument.
#' @param drop.gemetry drops geometry column if data is an sf data frame
#'
#' @return data
#' @export
#' @rdname internal
#' @keywords internal
check_data <- function(
    data = NULL,
    drop.geometry = FALSE,
    verbose = TRUE
){

  #check if it's NULL
  if(is.null(data)){
    stop("Argument 'data' is missing.")
  }

  if(!("data.frame" %in% class(data))){
    stop("Argument 'data' must be a data frame (tibbles and sf data frames are supported as well).")
  }

  #check number of rows
  if(nrow(data) < 30){
    if(verbose == TRUE){
      message("Argument 'data' has too few rows to fit a model.")
    }
  }

  if(drop.geometry == TRUE){
    if("sf" %in% class(data)){
      if(verbose == TRUE){
        message("Dropping geometry column from the 'data' data frame.")
      }
      data <- sf::st_drop_geometry(data)
    }
  }

  data

}


#' Checks 'predictors.names' argument
#'
#' @param data data argument.
#' @param predictors.names predictors.names.argument
#' @param numeric.only logical
#' @param is.required logical
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in data or not.
#' @param zero.variance.allowed logical
#' @param decimal.places integer, number of decimals for the zero variance test
#' @param verbose logical
#'
#' @return predictor.names
#' @export
#' @rdname internal
#' @keywords internal
check_predictors_names <- function(
    predictors.names = NULL,
    data = NULL,
    is.required = TRUE,
    numeric.only = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    decimal.places = 4,
    verbose = TRUE
){

  if(is.null(predictors.names) == TRUE){
    if(is.required == TRUE){
      stop("Argument 'predictors.names' is required.")
    }
  }

  if(is.character(predictors.names) == FALSE){
    stop("Argument 'predictors.names' must be a character vector.")
  }

  if(length(predictors.names) == 0){
    if(is.required == TRUE){
      stop("Argument 'predictors.names' is empty.")
    }
  }

  #check that all predictors are in data
  if(all(predictors.names %in% colnames(data)) == FALSE){

    if(verbose == TRUE){
      message(
        paste0(
          "The predictors.names ",
          paste0(
            predictors.names[!(predictors.names %in% colnames(data))],
            collapse = ", "
          ),
          " are missing from 'data'."
        )
      )
    }

    predictors.names <- predictors.names[predictors.names %in% colnames(data)]

  }

  #check that all predictors are numeric
  if(numeric.only == TRUE){

    non.numeric.predictors <- non_numeric_columns(
      data = data,
      columns = predictors.names
    )

    if(length(non.numeric.predictors) > 0){

      if(verbose == TRUE){
        message(
          "These non-numeric predictors will be ignored:\n",
          paste0(
            non.numeric.predictors,
            collapse = "\n"
          )
        )
      }

      predictors.names <- setdiff(
        predictors.names,
        non.numeric.predictors
      )

    }

  }

  #remove predictors with NA
  if(na.allowed == FALSE){

    na.columns <- apply(
      X = data[, predictors.names],
      MARGIN = 2,
      FUN = function(x){sum(is.na(x)) > 0}
    )

    if(all(na.columns) == FALSE){

      na.columns <- names(na.columns[na.columns])

      if(verbose == TRUE){
        message(
          "These predictors have NA and will be dropped:\n",
          paste0(
            na.columns,
            collapse = "\n"
          )
        )
      }

      predictors.names <- setdiff(
        predictors.names,
        na.columns
      )

    }

  }


  #removing columns with zero variance
  if(zero.variance.allowed == FALSE){

    zero.variance.columns <- zero_variance_columns(
      data = data,
      columns = predictors.names,
      decimal.places = decimal.places
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

      predictors.names <- setdiff(
        predictors.names,
        zero.variance.columns
      )

    }

  }


  predictors.names

}

#' Checks 'response.name' argument
#'
#' @param data data argument.
#' @param response.name response.name
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in data or not.
#' @param zero.variance.allowed logical
#' @param decimal.places integer, number of decimals for the zero variance test
#'
#' @return response.name
#' @export
#' @rdname internal
#' @keywords internal
check_response_name <- function(
    response.name = NULL,
    data = NULL,
    is.required = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    decimal.places = 4,
    verbose = TRUE
){

  if(is.null(response.name) == TRUE){
    if(is.required == TRUE){
      stop("Argument 'response.name' is required.")
    }
  }

  if(is.character(response.name) == FALSE){
    stop("Argument 'response.name' must be a character vector.")
  }

  if(length(response.name) != 1){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be of length 1 but it is empty.")
    }
  }

  #check that all predictors are in data
  if(!(response.name %in% colnames(data))){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be a column name of 'data'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response.name' must be a column name of 'data'.")
      }
    }
  }

  if(is.numeric(data[[response.name]]) == FALSE){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be a numeric column of 'data'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response.name' is not a numeric column of 'data' and will be ignored.")
      }
      return(NULL)
    }

  } else {

    if(zero.variance.allowed == FALSE){
      if(var(round(data[[response.name]], decimal.places)) == 0){
        if(is.required == TRUE){
          stop("Argument 'response.name' is the name of a column with near zero variance.")
        } else {
          if(verbose == TRUE){
            message("Argument 'response.name' is the name of a column with near zero variance. This might cause numerical issues.")
          }
        }
      }
    }

  }

  if(na.allowed == FALSE){

    if(sum(is.na(data[[response.name]])) > 0){
      if(is.required == TRUE){
        stop("Argument 'response.name' is the name of a column with NA values.")
      } else {
        if(verbose == TRUE){
          message("Argument 'response.name' is the name of a column with NA values. This might cause unintended issues.")
        }
      }
    }

  }



  response.name

}


#' Extract Numeric Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the numeric columns in the selected data frame.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
non_numeric_columns <- function(
    data,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  data <- data[, columns]

  out <- colnames(data)[!sapply(data, is.numeric)]

  out

}


#' Extract zero-variance Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the columns with zero variance.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @param decimal.places Integer, number of decimal places to round `columns` to. Defines the tolerance of the test. Default: 4
#' @return A character vector with the names of the numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
zero_variance_columns <- function(
    data,
    columns = NULL,
    decimal.places = 4
){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  numeric.columns <- numeric_columns(
    data = data,
    columns = columns
  )

  data <- data[, numeric.columns]

  zero.variance.columns <- colnames(data)[
    round(
      apply(
        X = data,
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
#' @param new.min (optional, numeric) New minimum value. Default: `0`
#' @param new.max (optional_numeric) New maximum value. Default: `1`
#' @param old.min (optional, numeric) Old minimum value. Default: `NULL`
#' @param old.max (optional_numeric) Old maximum value. Default: `NULL`
#' @return A numeric vector of the same length as x, but with its values rescaled between `new.min` and `new.max.`
#' @examples
#' if(interactive()){
#'
#'  out <- rescale_vector(
#'    x = rnorm(100),
#'    new.min = 0,
#'    new.max = 100,
#'    integer = TRUE
#'    )
#'    out
#'
#' }
#' @rdname rescale_vector
#' @export
rescale_vector <- function(x = NULL,
                           new.min = 0,
                           new.max = 1,
                           old.min = NULL,
                           old.max = NULL){

  if(is.null(x) | !is.vector(x) | !is.numeric(x)){
    stop("x must be a numeric vector.")
  }

  #data extremes
  if(is.null(old.min)){
    old.min = min(x)
  }

  if(is.null(old.max)){
    old.max = max(x)
  }

  #scaling
  x = ((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min

  x

}

#' Extract Non-numeric Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the non-numeric columns in the selected data frame.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the non-numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
numeric_columns <- function(
    data = NULL,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  data <- data[, columns]

  out <- colnames(data)[sapply(data, is.numeric)]

  out

}

