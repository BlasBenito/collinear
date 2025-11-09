#' Print \code{class.collinear_output}
#'
#' @param x (required, list of class \code{class.collinear_output}) Object to print. Default: NULL
#' @param n (optional, integer) Maximum printed vector length. Default: 5.
#' @param ... Ignored, kept for consistency with generic.
#' @method print collinear_output
#' @family S3_methods
#' @autoglobal
#' @export
print.collinear_output <- function(
    x = NULL,
    n = 5,
    ...
){

  if(length(names(x)) > 1){
    cat("Results\n")
    cat("===================\n")
    cat("\n")
  } else {
    cat("Result\n")
    cat("===================\n")
  }

  lapply(
    X = x,
    FUN = print,
    n = n
  )

  invisible()

}
