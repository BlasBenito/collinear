
#' Removes geometry column in sf data frames
#'
#' Replicates the functionality of  `\link[sf]{st_drop_geometry}` without depending on the `sf` package.
#'
#' @param df (required; data frame or matrix) Input data frame. Default: NULL
#'
#' @return The input data frame without a geometry column
#' @keywords internal
#' @autoglobal
#' @author Blas M. Benito
#' @export
drop_geometry_column <- function(df){

  #remove geometry column from df
  sf.column <- attributes(df)$sf_column

  if(!is.null(sf.column)){

    df <- as.data.frame(df)
    df[[sf.column]] <- NULL
    attr(df, "sf_column") <- NULL

  }

  df

}

