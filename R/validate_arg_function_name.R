#' Validates Argument \code{function_name}
#'
#' @description
#' Contatenates function names for a better message trace.
#'
#'
#' @param default_name (optional, character) Name of the calling function. Default: NULL
#' @param function_name (optional, character) Name of the parent function.
#' @inheritParams collinear
#'
#' @returns character
#' @family data_validation
#' @export
#' @autoglobal
validate_arg_function_name <- function(
    default_name = NULL,
    function_name = NULL,
    ...
){

  dots <- list(...)
  if(
    "function_name" %in% names(dots) &&
    is.null(function_name)
    ){
    function_name <- dots$function_name
  }

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
