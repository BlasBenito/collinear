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

  lapply(
    X = x[names(x) != "summary"],
    FUN = summary
  )

  invisible(x)

}
