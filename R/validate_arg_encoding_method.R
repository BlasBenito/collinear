#' Validate Argument \code{encoding_method}
#'
#' @inheritParams collinear
#' @inheritParams target_encoding_lab
#' @inheritParams validate_arg_quiet
#'
#' @returns character
#' @examples
#' x <- validate_arg_encoding_method(
#'   encoding_method = "wrong_method"
#'   )
#' @export
#' @autoglobal
validate_arg_encoding_method <- function(
    encoding_method = NULL,
    overwrite = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(is.null(function_name)){
    function_name <- "collinear::validate_arg_encoding_method()"
  }

  if(isTRUE(attr(x = encoding_method, which = "validated"))){
    return(encoding_method)
  }

  if(is.null(function_name)){
    function_name <- "collinear::validate_arg_encoding_method()"
  }

  if(is.null(encoding_method)){

    return(NULL)
  }

  valid_methods <- c(
    "mean",
    "loo",
    "rank"
  )

  encoding_method <- intersect(
    x = encoding_method,
    y = valid_methods
  )

  if(length(encoding_method) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'encoding_method' is not valid, resetting it to: '",
        valid_methods[1],
        "'."
      )

    }

    encoding_method <- valid_methods[1]

  }

  if(is.logical(overwrite) && overwrite == TRUE && length(encoding_method) > 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one encoding method allowed when 'overwrite = TRUE', using method: '",
        valid_methods[1],
        "'."
      )

    }

    encoding_method <- valid_methods[1]

  }

  attr(
    x = encoding_method,
    which = "validated"
  ) <- TRUE

  encoding_method


}
