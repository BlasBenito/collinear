#' Validate input data frame
#'
#' @description
#'
#' Internal function to validate and prepare the input data frame for a multicollinearity analysis.
#'
#' Validates a data frame to ensure it complies with the requirements of the package functions. The function performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item Stops if 'df' cannot be coerced to data frame.
#'   \item Stops if 'df' has zero rows.
#'   \item Removes geometry column if the input data frame is an "sf" object.
#'   \item Removes non-numeric columns with as many unique values as rows df has.
#'   \item Raise warning if number of rows of 'df' is lower than 'min_rows'.
#'   \item Converts logical columns to numeric.
#'   \item Converts factor and ordered columns to character.
#'   \item Tags the data frame with the attribute `validated = TRUE` to let the package functions skip the data validation.
#' }
#'
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#' @param min_rows (required; integer) Minimum number of rows required for a pairwise correlation or a variance inflation factor analysis. Default: 30
#' @return The input data frame modified to comply with the requirements of the functions in this package
#' @examples
#'
#' data(vi)
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #tagged as validated
#' attributes(vi)$validated
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
validate_df <- function(
    df = NULL,
    min_rows = 30
){

  #handle df = NULL
  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #if already validated, return it
  if(!is.null(attr(df, "validated"))){
    return(df)
  }

  #handle coercion to df
  if(is.data.frame(df) == FALSE){
    df <- tryCatch(
      {as.data.frame(df)},
      error = function(e){
        stop("argument 'df' must be a data frame or a matrix.")
      }
    )
  }

  #stop if no rows
  if(nrow(df) == 0){
    stop("Argument 'df' has zero rows.")
  }

  #remove geometry column from df
  df <- drop_geometry_column(df = df)

  #remove non-numeric columns with as many values as rows
  non.numeric.columns <- identify_non_numeric_predictors(df)

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
        "the column/s ",
        paste0(columns.to.remove, collapse = ", "),
        " have as many unique values as rows in 'df' and will be ignored."
      )
    }

    df <- df[, !(colnames(df) %in% columns.to.remove), drop = FALSE]

  }

  #convert types

  #logical to numeric
  df <- rapply(
    object = df,
    f = as.numeric,
    classes = c(
      "logical"
    ),
    how = "replace"
  )

  #factors and ordered to characters
  df <- rapply(
    object = df,
    f = as.character,
    classes = c(
      "factor",
      "ordered"
    ),
    how = "replace"
  )

  #number of rows must be > 30
  if(nrow(df) < min_rows){
    warning(
      "the number of rows in 'df' is lower than ",
      min_rows,
      ". A multicollinearity analysis may fail or yield meaningless results."
    )
  }

  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}

#' Validate the 'predictors' argument for analysis
#'
#' @description
#'
#' Requires the argument 'df' to be validated with [validate_df()].
#'
#' Validates the 'predictors' argument to ensure it complies with the requirements of the package functions. It performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item Stops if 'df' is not validated.
#'   \item If 'predictors' is NULL, uses column names of 'df' as 'predictors' in the 'df' data frame.
#'   \item Raise a warning if there are names in 'predictors' not in the column names of 'df', and returns only the ones in 'df'.
#'   \item Stop if the number of numeric columns in 'predictors' is smaller than 'min_numerics'.
#'   \item Raise a warning if there are zero-variance columns in 'predictors' and returns a new 'predictors' argument without them.
#'   \item Tags the vector with the attribute `validated = TRUE` to let the package functions skip the data validation.
#' }
#'
#'
#' @param df (required; data frame) A validated data frame with numeric and/or character predictors, and optionally, a response variable. Default: NULL.
#' @param response (optional, character string) Name of a numeric response variable. Used to remove the response from the predictors when predictors is NULL. Character response variables are ignored. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:NULL
#' @param min_numerics (required, integer) Minimum number of numeric predictors required. Default: 1
#' @param decimals (required, integer) Number of decimal places for the zero variance test. Smaller numbers will increase the number of variables detected as near-zero variance. Recommended values will depend on the range of the numeric variables in 'df'. Default: 4
#'
#' @return A character vector of validated predictor names
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #validating example predictors
#' vi_predictors <- validate_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' #tagged as validated
#' attributes(vi_predictors)$validated
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
validate_predictors <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    min_numerics = 0,
    decimals = 4
){

  #stop if no df
  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #stop if df is not validated,
  if(is.null(attr(df, "validated"))){
    stop("argument 'df' is not validated. Please, run validate_df() before validate_predictors().")
  }

  #if predictors is NULL, use colnames(df)
  if(is.null(predictors)){
    predictors <- colnames(df)
  }

  #if already validated, return it
  if(!is.null(attr(predictors, "validated"))){
    return(predictors)
  }

  #subset df
  df <- df[, predictors, drop = FALSE]

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
  if(length(identify_numeric_predictors(df)) < min_numerics){
    stop(
      "number of numeric columns in 'df' must be >= ",
      min_numerics,
      "."
    )
  }

  #removing zero variance predictors
  predictors.zero.variance <- identify_zero_variance_predictors(
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

  #removing response from predictors
  predictors <- setdiff(
    x = predictors,
    y = response
  )

  attr(
    x = predictors,
    which = "validated"
  ) <- TRUE

  predictors

}


#' Validate the 'response' argument for target encoding of non-numeric variables
#'
#' @description
#'
#' Requires the argument 'df' to be validated with [validate_df()].
#'
#'
#' @param df (required; data frame) A validated data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (optional, character string) Name of a numeric response variable. Character response variables are ignored. Default: NULL.
#' @param decimals (required, integer) number of decimal places for the zero variance test. Default: 4
#' @return character string with name of the response
#' @examples
#'
#' data(
#'   vi
#' )
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #validating example predictors
#' response <- validate_response(
#'   df = vi,
#'   response = "vi_mean"
#' )
#'
#' #tagged as validated
#' attributes(response)$validated
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
validate_response <- function(
    df = NULL,
    response = NULL,
    decimals = 4
){

  if(is.null(df)){
    stop("argument 'df' cannot be NULL.")
  }

  #if already validated, return it
  if(is.null(attr(df, "validated"))){
    stop("argument 'df' is not validated. Please, run validate_df() before validate_response().")
  }

  if(is.null(response) == TRUE){
    return(NULL)
  }

  #if already validated, return it
  if(!is.null(attr(response, "validated"))){
    return(response)
  }

  if(is.character(response) == FALSE){
    stop("argument 'response' must be a character string")
  }

  if(length(response) != 1){
    if(is.required == TRUE){
      stop("argument 'response' must be of length 1.")
    }
  }

  #check that the response is in df
  if(!(response %in% colnames(df))){
    stop("argument 'response' must be a column name of 'df'.")
  }

  if(is.numeric(df[[response]]) == FALSE){
    warning(
      "the 'response' column '",
      response,
      "' is not numeric, ignoring it."
    )
    return(NULL)
  }

  response.zero.variance <- identify_zero_variance_predictors(
    df = df,
    predictors = response,
    decimals = 4
  )

  if(length(response.zero.variance) == 1){
    warning(
      "the 'response' column '",
      response,
      "' has near-zero variance, ignoring it."
    )
    return(NULL)
  }

  response.na.values <- sum(is.na(df[[response]]))
  if(response.na.values > 0){
    warning(
      "the 'response' column '",
      response,
      "' has ",
      response.na.values,
      " NA values. This may cause unexpected issues."
    )
  }

  attr(
    x = response,
    which = "validated"
  ) <- TRUE

  response

}








