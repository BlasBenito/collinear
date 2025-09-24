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
#' @family data_validation
#' @export
validate_arg_quiet <- function(
    function_name = NULL,
    quiet = NULL
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_quiet()",
    function_name = function_name
  )

  if(is.logical(quiet)){
    return(quiet)
  }

  message(
    "\n",
    function_name,
    ": argument 'quiet' must be logical, setting it to FALSE."
  )

  FALSE

}
