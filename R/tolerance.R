#' Tolerance for Detecting Linear Dependencies
#'
#' @description
#' Internal function to define the \code{tol} argument in [solve()]. Returns \code{0} if the system has long-double capabilities, and \code{.Machine$double.eps} otherwise.
#'
#'
#' @return numeric
#' @autoglobal
#' @export
#' @family data_validation
#' @examples
#' tolerance()
tolerance <- function(){

  if(capabilities("long.double") == FALSE){
    return(.Machine$double.eps)
  }

  0

}
