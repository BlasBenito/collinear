#' Validate Argument \code{quiet}
#'
#' @description
#' Internal function to validate the logical argument \code{quiet}, which triggers messaging when \code{FALSE}
#'
#' @inheritParams collinear
#' @param function_name (optional, character string) Name of the function performing the argument check. Default: NULL
#' @return logical
#' @examples
#'
#' x <- validate_arg_quiet(
#'   quiet = TRUE
#'   )
#'
#' attributes(x)$validated
#'
#' @autoglobal
#' @family argument_validation
#' @export
validate_arg_quiet <- function(
  quiet = FALSE,
  function_name = NULL
) {
  if (isTRUE(attr(x = quiet, which = "validated"))) {
    return(quiet)
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_quiet()",
    function_name = function_name
  )

  if (is.logical(quiet)) {
    return(quiet)
  }

  message(
    "\n",
    function_name,
    ": argument 'quiet' must be logical, setting it to FALSE."
  )

  quiet <- FALSE

  attr(
    x = quiet,
    which = "validated"
  ) <- TRUE

  quiet
}
