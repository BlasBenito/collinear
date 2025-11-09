#' Removes \code{geometry} Column From \code{sf} Dataframes
#'
#' Replicates the functionality of \code{sf::st_drop_geometry()} without depending on the \code{sf} package.
#'
#' @inheritParams collinear
#'
#' @return dataframe
#' @family data_preparation
#' @autoglobal
#' @author Blas M. Benito, PhD
#' @export
#' @examples
#' data(vi_smol)
#'
#' #creating fake geometry column without sf loaded
#' vi_smol$geometry <- NA
#' attr(
#'   x = vi_smol,
#'   which = "sf_column"
#'   ) <- "geometry"
#'
#' #check new attribute
#' attributes(vi_smol)$sf_column
#'
#' #drop geometry column
#' df <- drop_geometry_column(
#'   df = vi_smol
#'   )
#'
#' #checking that the geometry was droppped
#' "geometry" %in% colnames(df)
#' attributes(df)$sf_column
#'
drop_geometry_column <- function(
    df = NULL,
    quiet = FALSE,
    ...
    ){

  function_name <- validate_arg_function_name(
    default_name = "collinear::drop_geometry_column()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #remove geometry column from df
  sf.column <- attributes(df)$sf_column

  if(!is.null(sf.column)){

    if(quiet == FALSE){

      message("\n",
      function_name,
      ": dropping geometry column from 'df'.")

    }

    df <- as.data.frame(df)
    df[[sf.column]] <- NULL
    attr(df, "sf_column") <- NULL

  }

  df

}

