#' Removes geometry column in sf data frames
#'
#' Replicates the functionality of  `sf::st_drop_geometry()` without depending on the `sf` package.
#'
#' @inheritParams collinear
#'
#' @return data frame
#' @family data_preparation
#' @autoglobal
#' @author Blas M. Benito, PhD
#' @export
drop_geometry_column <- function(
    df = NULL,
    quiet = FALSE
    ){

  #remove geometry column from df
  sf.column <- attributes(df)$sf_column

  if(!is.null(sf.column)){

    if(quiet == FALSE){

      message("\ncollinear::drop_geometry_column(): dropping geometry column from 'df'.")

    }

    df <- as.data.frame(df)
    df[[sf.column]] <- NULL
    attr(df, "sf_column") <- NULL

  }

  df

}

