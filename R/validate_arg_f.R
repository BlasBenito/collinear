#' Validate Argument \code{f}
#'
#' @inheritParams preference_order
#' @inheritParams validate_arg_df
#' @param f_name (optional, string) Name of the function \code{f}, as returned by \code{deparse(substitute(f))}. Default: NULL
#'
#' @return function
#' @autoglobal
#' @family data_validation
#' @export
#' @examples
#' x <- validate_arg_df(df = f_auto)
validate_arg_f <- function(
    f = NULL,
    f_name = NULL,
    function_name = NULL
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_f()",
    function_name = function_name
  )

  #NULL
  if(is.null(f)){
    return(NULL)
  }

  #valid
  if(!is.null(attributes(f)$validated)){
    return(f)
  }

  #is function
  if (!is.function(f)) {
    stop(
      "\n",
      function_name,
      ": argument 'f' must be a uquoted function name without parentheses.",
      call. = FALSE
    )
  }

  #has 'df' argument
  f_args <- names(formals(f))
  if (!"df" %in% f_args) {
    stop(
      "\n",
      function_name,
      ": the function 'f' must have the argument 'df' to receive a data frame with the column names 'x' and 'y'.",
      call. = FALSE
    )
  }

  #attributes
  if(!is.null(f_name)){
    attr(x = f, which = "name") <- f_name
  }

  attr(x = f, which = "validated") <- TRUE


  f
}
