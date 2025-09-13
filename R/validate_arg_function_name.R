#' Validates Argument \code{function_name}
#'
#' @description
#' Contatenates function names for a better message trace.
#'
#'
#' @param default_name (optional, character) Name of the calling function. Default: NULL
#' @param function_name (optional, character) Name of the parent function.
#'
#' @returns character
#' @export
#' @autoglobal
validate_arg_function_name <- function(
    default_name = "hola",
    function_name = "adios"
){

  if(all(is.null(c(default_name, function_name)))){
    return(NULL)
  }

  if(is.null(function_name) && !is.null(default_name)){
    return(default_name)
  }

  hirarchy_symbol <- "└── "
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

  message(function_name)


}
