#' Removes geometry column in sf data frames
#'
#' Replicates the functionality of \code{sf::st_drop_geometry()} without depending on the \code{sf} package.
#'
#' @inheritParams collinear
#'
#' @return data frame
#' @family data_preparation
#' @autoglobal
#' @author Blas M. Benito, PhD
#' @export
#' @examples
#' data(vi)
#'
#' #creating fake geometry column without sf loaded
#' vi$geometry <- NA
#' attr(
#'   x = vi,
#'   which = "sf_column"
#'   ) <- "geometry"
#'
#' #drop geometry column
#' df <- drop_geometry_column(
#'   df = vi
#'   )
#'
#' "geometry" %in% colnames(df)
#'
drop_geometry_column <- function(
    df = NULL,
    quiet = FALSE,
    ...
    ){

  dots <- list(...)
  if("function_name" %in% names(dots)){
    function_name <- dots$function_name
    dots$function_name <- NULL
  } else {
    function_name <- NULL
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::drop_geometry_column()",
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
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

