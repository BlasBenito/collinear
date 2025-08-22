#' Summary Method for Class \code{collinear_output}
#'
#' @param x (required, list) Object of class \code{collinear_output} resulting from [collinear()]. Default: NULL
#'
#' @method summary collinear_output
#' @autoglobal
#' @export
summary.collinear_output <- function(
    x = NULL
){

  if(!is.null(x$arguments)){
    x$arguments <- NULL
  }

  lapply(
    X = x,
    FUN = summary
  )

  invisible(x)

}
