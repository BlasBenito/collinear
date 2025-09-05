#' Print \code{collinear_output}
#' @param x (required, list of class \code{collinear_output}) Object to print. Default: NULL
#' @param n (optional, integer) Maximum printed vector length. Default: 5.
#' @param ... Ignored, kept for consistency with generic.
#' @method print collinear_output
#' @export
print.collinear_output <- function(
    x = NULL,
    n = 5,
    ...
){

  if(!is.null(x$arguments)){
    print(x$arguments)
  }

  y <- x[names(x) != "arguments"]

  if(length(names(y)) > 1){
    cat("Results\n")
    cat("===================\n")
    cat("\n")
  } else {
    cat("Result\n")
    cat("===================\n")
  }

  lapply(
    X = x[names(x) != "arguments"],
    FUN = print,
    n = n
  )

  invisible()

}
