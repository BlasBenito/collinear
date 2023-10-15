#' Target encode a categorical variable based on a numeric variable.
#'
#' Transforms categorical variables into numeric by assigning to each category the mean of a numeric variable (usually the model response) across the group's cases.
#'
#' This function is used by default in [collinear()], [cor_select()] and [vif_select()]. If you wish to have more control over the target encoding process, you can use [target_encoding_lab()].
#'
#' @param response (required; numeric vector) A numeric vector generally representing a response variable. Default: NULL
#' @param predictor (required; character vector) A character vector of the same length as 'response', representing the categorical variable.  Default: NULL
#' @param check_input (required; logical) If FALSE, disables data checking for a slightly better performance when many tests are performed. Default: TRUE
#'
#' @return A numeric vector containing target-encoded values.
#'
#' @examples
#' if(interactive()){
#'
#' #load example data frame
#' data(vi)
#'
#' biogeo_biome_encoded <- mean_encoder(
#'   response = vi$vi_mean,
#'   predictor = vi$biogeo_biome
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
mean_encoder <- function(
    response = NULL,
    predictor = NULL,
    check_input = TRUE
){

  #data checks
  if(check_input == TRUE){

    if(is.null(response)){
      stop("argument 'response' must not be NULL.")
    }

    if(is.null(predictor)){
      stop("argument 'predictor' must not be NULL.")
    }

    if(length(response) != length(predictor)){
      stop("arguments 'response' and 'predictor' must have the same length.")
    }

    response <- as.numeric(response)
    if(is.numeric(response) == FALSE){
      stop("argument 'response' must be of type numeric.")
    }

    predictor <- as.character(predictor)
    if(is.character(predictor) == FALSE){
      stop("argument 'predictor' must be of type character")
    }

  }

  #to data frame
  df <- data.frame(
    character = as.character(predictor),
    numeric = as.numeric(response)
  )

  #target encoding
  df <- df |>
    dplyr::group_by(character) |>
    dplyr::mutate(
      encoded = mean(
        numeric,
        na.rm = TRUE
      )
    )

  df[["encoded"]]

}

