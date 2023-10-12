#' Internal function to inspect the df argument
#'
#' @description
#' Performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item Stops if 'df' cannot be coerced to data frame.
#'   \item Removes geometry column if required.
#'   \item Removes non-numeric columns with as many unique values as rows df has.
#'   \item Raise warning if number of rows of 'df' is lower than 'min_rows'.
#' }
#'
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param min_rows (required; integer) Minimum number of rows required for a pairwise correlation or a variance inflation factor analysis. Default: 30
#' @return data frame
#' @examples
#' if (interactive()) {
#'   data(ecoregions)
#'   ecoregions <- df_inspect(df = ecoregions)
#'
#' }
#' @keywords internal
#' @export
df_inspect <- function(
    df = NULL,
    min_rows = 30
){

  #handle df = NULL
  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  #if already inspected, return it
  if(!is.null(attr(df, "inspected"))){
    return(df)
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

  #remove geometry column from df
  df <- df_drop_geometry(df = df)

  #remove non-numeric columns with as many values as rows
  non.numeric.columns <- predictors_character(df)

  if(length(non.numeric.columns) > 0){

    non.numeric.columns.unique.values <- lapply(
      X = non.numeric.columns,
      FUN = function(x) length(unique(df[[x]]))
    ) |>
      unlist()

    names(non.numeric.columns.unique.values) <- non.numeric.columns

    columns.to.remove <- names(
      non.numeric.columns.unique.values[
        non.numeric.columns.unique.values == nrow(df)
      ]
    )

    if(length(columns.to.remove) > 0){
      warning(
        "The column/s ",
        paste0(columns.to.remove, collapse = ", "),
        " have as many unique values as rows in 'df' and will be ignored."
      )
    }

    df <- df[, !(colnames(df) %in% columns.to.remove)]

  }



  #number of rows must be > 30
  if(nrow(df) < min_rows){
    warning(
      "Number of rows in 'df' is lower than ",
      min_rows,
      ". This function may fail or yield meaningless results."
    )
  }

  attr(
    x = df,
    which = "inspected"
  ) <- TRUE

  df

}

#' Internal function to check the 'predictors' argument
#'
#' @description
#' Performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item If 'predictors' is NULL, uses column names of 'df' as 'predictors'in df data frames.
#'   \item Removes column "geometry" (in sf data frames).
#'   \item Raise warning if there are names in 'predictors' not in the column names of 'df'.
#'   \item Stops if number of numeric columns in 'predictors' is smaller than 'min_numerics'.
#'   \item Raise warning if there are zero-variance columns in 'predictors' and ignores them.
#' }
#'
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param predictors (optional; character vector) names of predictors in arguemnt 'df'. Default: NULL
#' @param min_numerics (required, integer) Minimum number of numeric predictors required. Default: 1
#' @param decimals (required, integer) number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4
#'
#' @return predictor names
#' @keywords internal
#' @autoglobal
#' @export
predictors_inspect <- function(
    df = NULL,
    predictors = NULL,
    min_numerics = 1,
    decimals = 4
){

  #stop if no df
  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  #if predictors is NULL, use colnames(df)
  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  #subset df
  df <- df[, predictors]

  #if already inspected, return it
  if(!is.null(attr(predictors, "inspected"))){
    return(predictors)
  }

  #identify wrongly named predictors
  predictors.missing <- setdiff(
    x = predictors,
    y = colnames(df)
  )

  if(length(predictors.missing) > 0){

    warning(
      "these predictors are not column names of 'df' and will be ignored:\n",
      paste(
        predictors.missing,
        collapse = "\n"
      )
    )

    #getting common predictors
    predictors <- intersect(
      x = predictors,
      y = colnames(df)
    )

  }

  #number of numeric predictors must be >= min_numerics
  if(length(predictors_numeric(df)) < min_numerics){
    stop(
      "number of numeric columns in 'df' must be >= ",
      min_numerics,
      "."
    )
  }

  #removing zero variance predictors
  predictors.zero.variance <- predictors_zero_variance(
    df = df,
    predictors = predictors,
    decimals = decimals
  )

  if(length(predictors.zero.variance) > 0){

    warning(
      "these predictors have near zero variance and will be ignored:\n",
      paste0(
        predictors.zero.variance,
        collapse = "\n"
      )
    )

    predictors <- setdiff(
      predictors,
      predictors.zero.variance
    )

  }

  attr(
    x = predictors,
    which = "inspected"
  ) <- TRUE

  predictors

}


#' Return names of non-numeric predictors
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param predictors (optional, character vector) names of predictors in 'df'. Default: NULL
#' @return character vector with names of non-numeric predictors.
#' @keywords internal
#' @autoglobal
#' @export
predictors_numeric <- function(
    df = NULL,
    predictors = NULL
){

  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  #remove geometry column from df
  df <- df_drop_geometry(df = df)

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  df <- df[, predictors]

  colnames(df)[sapply(df, is.numeric)]

}

#' Extract non-numeric predictors from a data frame
#'
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param predictors (optional, character vector) names of predictors in arguemnt 'df'. Default: NULL
#' @return character vector with names of non-numeric columns.
#' @keywords internal
#' @autoglobal
#' @export
predictors_character <- function(
    df = NULL,
    predictors = NULL
){

  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  #remove geometry column from df
  df <- df_drop_geometry(df = df)

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  df <- df[, predictors]

  colnames(df)[!sapply(df, is.numeric)]

}


#' Extract zero-variance predictors from a data frame
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param predictors (optional, character vector) names of predictors in arguemnt 'df'. Default: NULL
#' @param decimals (required, integer) number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4
#' @return character vector with names of zero-variance columns.
#' @keywords internal
#' @autoglobal
#' @export
predictors_zero_variance <- function(
    df = NULL,
    predictors = NULL,
    decimals = 4
){

  if(is.null(df)){
    stop("Argument 'df' cannot be NULL.")
  }

  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  predictors.numeric <- predictors_numeric(
    df = df,
    predictors = predictors
  )

  df <- df[, predictors.numeric]

  colnames(df)[
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

}



#' Checks the 'response' argument
#'
#' @param df (required; data frame or matrix) input data frame. Default: NULL
#' @param response (optional, character string) Name of a numeric response variable. Character response variables are ignored. Default: NULL.
#' @param decimals (required, integer) number of decimal places to round `predictors` to. Defines the tolerance of the zero-variance test. Default: 4
#' @return character string with name of the response
#' @keywords internal
#' @autoglobal
#' @export
response_inspect <- function(
    df = NULL,
    response = NULL,
    decimals = 4
){

  if(is.null(response) == TRUE){
    return(NULL)
  }

  if(is.character(response) == FALSE){
    stop("Argument 'response' must be a character string")
  }

  if(length(response) != 1){
    if(is.required == TRUE){
      stop("Argument 'response' must be of length 1.")
    }
  }

  #check that the response is in df
  if(!(response %in% colnames(df))){
    stop("Argument 'response' must be a column name of 'df'.")
  }

  if(is.numeric(df[[response]]) == FALSE){
    warning(
      "The 'response' column '",
      response,
      "' is not numeric, ignoring it."
    )
    return(NULL)
  }

  response.zero.variance <- predictors_zero_variance(
    df = df,
    predictors = response,
    decimals = 4
  )

  if(length(response.zero.variance) == 1){
    warning(
      "The 'response' column '",
      response,
      "' has near-zero variance, ignoring it."
    )
    return(NULL)
  }

  response.na.values <- sum(is.na(df[[response]]))
  if(response.na.values > 0){
    warning(
      "The 'response' column '",
      response,
      "' has ",
      response.na.values,
      " NA values. This may cause unexpected issues."
    )
  }

  response

}







#' @title Rescales a numeric vector into a new range
#' @param x (required, numeric vector) Numeric vector. Default: NULL
#' @param new_min (optional, numeric) New minimum value. Default: 0
#' @param new_max (optional_numeric) New maximum value. Default: 1
#' @param old_min (optional, numeric) Old minimum value. Default: NULL
#' @param old_max (optional_numeric) Old maximum value. Default: NULL
#' @return numeric vector with rescaled values of x.
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
#' @keywords internal
#' @autoglobal
#' @export
rescale_vector <- function(
    x = NULL,
    new_min = 0,
    new_max = 1,
    old_min = NULL,
    old_max = NULL
){

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


#' Removes geometry column in sf data frames
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#'
#' @return The input data frame without a geometry column
#' @keywords internal
#' @autoglobal
#' @export
df_drop_geometry <- function(df){

  #remove geometry column from df
  sf.column <- attributes(df)$sf_column

  if(!is.null(sf.column)){

    df <- as.data.frame(df)
    df[[sf.column]] <- NULL
    attr(df, "sf_column") <- NULL

  }

  df

}

