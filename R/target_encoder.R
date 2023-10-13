#' Target encode a categorical variable based on a numeric variable.
#'
#' Transforms character variables into numeric by assigning to each group the mean of a numeric variable (usually the model response) across the group's cases.
#'
#' @param x (required; numeric vector) A numeric vector generally representing a response variable. Default: NULL
#' @param y (required; character vector) A character vector of the same length as 'x', representing the categorical variable.  Default: NULL
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
#' biogeo_biome_encoded <- target_encoder(
#'   x = vi$vi_mean,
#'   y = vi$biogeo_biome
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
target_encoder <- function(
    x = NULL,
    y = NULL,
    check_input = TRUE
){

  #data checks
  if(check_input == TRUE){

    if(is.null(x)){
      stop("argument 'x' must not be NULL.")
    }

    if(is.null(y)){
      stop("argument 'y' must not be NULL.")
    }

    if(length(x) != length(y)){
      stop("arguments 'x' and 'y' must have the same length.")
    }

    x <- as.numeric(x)
    if(is.numeric(x) == FALSE){
      stop("argument 'x' must be of type numeric.")
    }

    y <- as.character(y)
    if(is.character(y) == FALSE){
      stop("argument 'y' must be of type character")
    }

  }

  #to data frame
  df <- data.frame(
    character = as.character(y),
    numeric = as.numeric(x)
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

