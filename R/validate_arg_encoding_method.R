#' Validate Argument \code{encoding_method}
#'
#' @description
#' Internal function to validate the argument \code{encoding_method} of [target_encoding_lab()].
#'
#'
#' @inheritParams target_encoding_lab
#' @inheritParams validate_arg_quiet
#'
#' @returns character
#' @family argument_validation
#' @examples
#' x <- validate_arg_encoding_method(
#'   encoding_method = "wrong_method"
#'   )
#' @export
#' @autoglobal
validate_arg_encoding_method <- function(
    encoding_method = c(
      "loo",
      "mean",
      "rank"
    ),
    overwrite = NULL,
    quiet = FALSE,
    function_name = NULL
){

  valid_methods <- c(
    "loo",
    "mean",
    "rank"
  )

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_encoding_method()",
    function_name = function_name
  )

  if(is.null(encoding_method)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'encoding_method' is NULL, skipping target encoding."
      )

    }

    return(NULL)

  }

  if(isTRUE(attr(x = encoding_method, which = "validated"))){
    return(encoding_method)
  }

  encoding_method <- intersect(
    x = encoding_method,
    y = valid_methods
  )

  if(
    is.null(encoding_method) ||
    length(encoding_method) == 0
    ){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'encoding_method' is not valid, resetting it to '",
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
