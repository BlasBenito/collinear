#' Validate Argument \code{function_name}
#'
#' @description
#' Concatenates parent and child function names for a better message, warning, and error tracing.
#'
#'
#' @param default_name (optional, character) Name of the calling function. Default: NULL
#' @param function_name (optional, character) Name of the parent function. Default: NULL
#' @param ... (optional) Used to pass \code{function_name} within these functions that don't have this argument.
#'
#' @returns character
#' @export
#' @family argument_validation
#' @examples
#' x <- validate_arg_function_name(
#'   default_name = "child_function",
#'   function_name = "parent_function"
#' )
#'
#' message(x)
#' @autoglobal
validate_arg_function_name <- function(
    default_name = NULL,
    function_name = NULL,
    ...
){

  if(all(is.null(c(default_name, function_name)))){
    return(NULL)
  }

  if(is.null(function_name) && !is.null(default_name)){
    return(default_name)
  }

  hirarchy_symbol <- "\u2514\u2500\u2500 "
  spaces <- "    "

  spaces_multiplier <- length(
    regmatches(
      x = function_name,
      m = gregexpr(
        pattern = hirarchy_symbol,
        text = function_name
      )
    )[[1]]
  )

  spaces <- paste0(
    rep(x = "    ", times = spaces_multiplier),
    collapse = ""
  )

  function_name <- paste0(
    function_name,
    "\n",
    spaces,
    hirarchy_symbol,
    default_name
  )

  function_name


}
