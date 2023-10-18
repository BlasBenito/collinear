#' Target encoding of categorical variables in a data frame
#'
#' Applies [mean_encoder()] to all non-numeric columns in a data frame. Used within [collinear()], [cor_select()] and [vif_select()]. If you wish to have more control over the target encoding process, you can use [target_encoding_lab()].
#'
#' This function performs target encoding of categorical variables (including factor, ordered, and logical types) based on a numeric response variable. This method facilitates a seamless computation of multicollinearity metrics between numeric and non-numeric variables when a numeric response is available.
#'
#' Mean encoding is a method of target encoding used to transform categorical variables into numeric representations based on their relationship with a target variable (usually, the model's response). This method calculates the mean of the target variable for each category within a categorical feature.
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
#'   vi <- collinear::mean_encode(
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
mean_encode <- function(
    df = NULL,
    response = NULL,
    predictors = NULL
){

  #do nothing if response is NULL
  if(is.null(response)){
    return(df)
  }

  #validate input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #validate predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
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
        ~mean_encoder(
          response = df[[response]],
          predictor = .,
          check_input = FALSE
        )
      )
    )

  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}
