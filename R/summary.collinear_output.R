#' Summary of \code{class.collinear_output}
#'
#' @param object (required, list of class \code{collinear_output}) Object to summarize. Default: NULL
#' @param ... Ignored, kept for consistency with generic.
#' @return list:
#' If \code{object} was created with \code{responses = NULL}, a sublist named "result" containing a vector with the selected predictors. Otherwise, a list named after each response containing the corresponding variable selection.
#' @method summary collinear_output
#' @family S3_methods
#' @autoglobal
#' @export
summary.collinear_output <- function(
  object = NULL,
  ...
) {
  #print summary of selections
  lapply(
    X = object,
    FUN = summary
  )
}
