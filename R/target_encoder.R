#' Target encode a categorical variable based on a numeric variable.
#'
#' This function calculates the mean of the numeric variable for each group defined by
#' the categorical variable and returns the target-encoded values.
#'
#' @param x (required; numeric vector) A numeric vector. Default: NULL
#' @param y (required; character vector) A character vector of the same length as 'x', representing the categorical variable.  Default: NULL
#' @param check_input (required; logical) If FALSE, disables data checking for a slightly better performance when many tests are performed. Default: TRUE
#'
#' @return A numeric vector containing target-encoded values.
#'
#' @examples
#' if(interactive()){
#' data(ecoregions)
#' encoded_values <- target_encoder(
#'   x = ecoregions$plant_richness,
#'   y = ecoregions$dominant_landcover
#'   )
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

    #checking equal length
    if(length(x) != length(y)){
      stop("Arguments 'x' and 'y' must have the same length.")
    }

    #checking x
    if(is.null(x)){
      stop("The argument 'x' must not be NULL.")
    }

    if(is.numeric(as.numeric(x)) == FALSE){
      stop("Argument 'x' must be of type numeric.")
    }

    #checking y
    if(is.null(y)){
      stop("The argument 'y' must not be NULL.")
    }

    if(is.character(as.character(y)) == FALSE){
      stop("Argument 'y' must be of type character")
    }

  }

  #to data frame
  df <- data.frame(
    character = as.character(y),
    numeric = as.numeric(x)
  )

  #dplyr version
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

