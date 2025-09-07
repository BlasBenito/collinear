#' Summary of \code{collinear_output}
#' @param object (required, list of class \code{collinear_output}) Object to summarize. Default: NULL
#' @param ... Ignored, kept for consistency with generic.
#' @method summary collinear_output
#' @family S3_methods
#' @autoglobal
#' @export
summary.collinear_output <- function(
    object = NULL,
    ...
){

  x <- object[names(object) != "arguments"]

  #print summary of selections
  x.summary <- lapply(
    X = x,
    FUN = summary
  )

  class(x.summary) <- c(class(x.summary), "summary.collinear_output")

  x.summary

}
