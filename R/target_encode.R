#' Target encoding of categorical variables in a data frame
#'
#' This function performs target encoding of categorical variables (including factor, ordered, and logical types) based on a numeric response variable. This method facilitates a seamless computation of multicollinearity metrics between numeric and non-numeric variables when a numeric response is available.
#'
#' Target encoding, also known as mean encoding, is a technique used to transform categorical variables into numeric representations based on their relationship with a target variable (usually, the model's response). This method calculates the mean of the target variable for each category within a categorical feature. Target encoding is particularly useful when working with categorical variables that exhibit strong associations with the target variable, making it easier for models to learn from the encoded information.
#'
#'
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#'
#' @return A data frame with its categorical variables converted to numeric via target encoding.
#' @examples
#' if (interactive()) {
#'
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
#'     df = vi,
#'     predictors = vi_predictors
#'     )
#'
#'   #validate the response "vi_mean"
#'   response <- collinear::validate_response(
#'     df = vi,
#'     response = "vi_mean"
#'     )
#'
#'   #names of non-numeric predictors
#'   predictors.non.numeric <- collinear::identify_non_numeric_predictors(
#'     df = vi,
#'     predictors = predictors
#'   )
#'
#'   #target encoding
#'   vi <- collinear::target_encode(
#'     df = vi,
#'     response = response,
#'     predictors = predictors
#'   )
#'
#'   #names of non-numeric predictors again
#'   predictors.non.numeric <- collinear::identify_non_numeric_predictors(
#'     df = vi,
#'     predictors = predictors
#'   )
#'
#' }
#' @export
#' @autoglobal
target_encode <- function(
    df = NULL,
    response = NULL,
    predictors = NULL
){

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- validate_response(
    df = df,
    response = response
  )

  #stop if NULL or non-numeric.
  if(is.null(response)){
    stop("the argument 'response' must name a numeric column of 'df'.")
  }

  #factors, logical, and ordered to characters
  df <- rapply(
    object = df[, c(response, predictors)],
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  predictors.character <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(is.null(response)){
    if(length(predictors.character) > 0){
      warning("please provide a numeric 'response' variable to target-encode all non-numeric predictors in 'predictors'.")
    }
    return(df)
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(predictors.character),
        ~target_encoder(
          x = df[[response]],
          y = .,
          check_input = FALSE
        )
      )
    )

  df

}
