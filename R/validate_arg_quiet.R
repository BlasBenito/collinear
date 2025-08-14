#' Validate Argument \code{quiet}
#'
#' @description
#' Internal function to validate the argument \code{quiet}.
#'
#' @inheritParams collinear
#' @param function_name (optional, character string) Name of the function performing the argument check. Default: NULL
#' @return logical
#' @examples
#'
#' quiet <- validate_arg_quiet(
#'   function_name = "f()",
#'   quiet = TRUE
#'   )
#'
#' attributes(quiet)$validated
#'
#' @autoglobal
#' @export
validate_arg_quiet <- function(
    function_name = NULL,
    quiet = NULL
){

  if(is.logical(quiet)){
    return(quiet)
  }

  FALSE

}
