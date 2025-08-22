#' Print Method for Class \code{collinear_output}
#'
#' Prints objects of the class [collinear_output] produced by [collinear()].
#'
#' @param x (required, list) Object of class \code{collinear_output} resulting from [collinear()]. Default: NULL
#' @inheritParams print.collinear_selection
#'
#' @method print collinear_output
#' @autoglobal
#' @export
print.collinear_output <- function(
    x = NULL,
    n = 5
){

  lapply(
    X = x,
    FUN = print,
    n = n
  )

  invisible(x)

}
