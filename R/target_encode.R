#' Target encode a categorical variable based on a numeric variable.
#'
#' This function calculates the mean of the numeric variable for each group defined by
#' the categorical variable and returns the target-encoded values.
#'
#' @param numeric (required; numeric vector) A numeric vector.
#' @param character (required; character vector) A character vector of the same length as 'numeric', representing the categorical variable.
#'
#' @return A numeric vector containing target-encoded values.
#'
#' @examples
#' if(interactive()){
#' data(ecoregions)
#' encoded_values <- target_encode(
#'   numeric = ecoregions$plant_richness,
#'   character = ecoregions$dominant_landcover
#'   )
#' }

#'
#' @autoglobal
#' @export
target_encode <- function(
    numeric = NULL,
    character = NULL
){

  #checking equal length
  if(length(numeric) != length(character)){
    stop("Arguments 'numeric' and 'character' must have the same length.")
  }

  #checking numeric
  numeric <- as.numeric(numeric)

  if(is.null(numeric)){
    stop("The argument 'numeric' must not be NULL.")
  }

  if(is.vector(numeric) == FALSE){
    stop("Argument 'numeric' must be a numeric vector.")
  }

  if(is.numeric(numeric) == FALSE){
    stop("Argument 'numeric' must be of type numeric.")
  }

  #checking character
  character <- as.character(character)

  if(is.null(character)){
    stop("The argument 'character' must not be NULL.")
  }

  if(is.vector(character) == FALSE){
    stop("Argument 'character' must be a character vector.")
  }

  if(is.character(character) == FALSE){
    stop("Argument 'character' must be of type character")
  }

  #to data frame
  df <- data.frame(
    character = character,
    numeric = numeric
  )

  #target encoding code
  if(
    requireNamespace(
      package = "data.table",
      quietly = TRUE
    )
  ){

    #data.table version
    ############################
    `:=` <- data.table::`:=`

    #combine as data table
    df <- data.table::as.data.table(df)

    #compute mean per group
    df[,
       encoded := mean(numeric, na.rm = TRUE),
       by = character
    ]

  } else {

    #dplyr version
    df <- df |>
      dplyr::group_by(character) |>
      dplyr::mutate(
        encoded = mean(
          numeric,
          na.rm = TRUE
          )
      )

  }

  df[["encoded"]]

}

