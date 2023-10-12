#' Target encoding of 'df' argument
#'
#' @param df (required; data frame or tibble) A data frame Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If ommited, all columns of 'df' are used as predictors. Default:'NULL'
#'
#' @return data frame
#' @export
#' @autoglobal
target_encode <- function(
    df = NULL,
    response = NULL,
    predictors = NULL
){

  #check input data frame
  df <- df_inspect(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- predictors_inspect(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- response_inspect(
    df = df,
    response = response
  )

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

  predictors.character <- predictors_character(
    df = df,
    predictors = predictors
  )

  if(is.null(response)){
    if(length(predictors.character) > 0){
      warning("Please provide a numeric 'response' variable to target-encode all character predictors in 'df'.")
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
