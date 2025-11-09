#' Validate Argument \code{max_cor}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return numeric or NULL
#' @autoglobal
#' @export
#' @family argument_validation
#' @examples
#' x <- validate_arg_max_cor(
#'   max_cor = 1.5, #wrong value
#'   quiet = FALSE
#' )
#'
#' x
#' attributes(x)$validated
validate_arg_max_cor <- function(
    max_cor = NULL,
    quiet = FALSE,
    function_name = NULL
    ){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_max_cor()",
    function_name = function_name
  )

  max_cor_default <- 0.70

  if(isTRUE(attr(x = max_cor, which = "validated"))){
    return(max_cor)
  }

  if(is.null(max_cor)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is NULL, skipping correlation filtering."
      )

    }

    return(NULL)

  }

  if(is.numeric(max_cor)){

    if(length(max_cor) > 1){

      max_cor <- max_cor[1]

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'max_cor' must be of length one, using value '", max_cor, "'."
        )

      }

    }

  } else {

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is non-numeric, resetting it to '", max_cor_default, "'."
      )

    }

    max_cor <- max_cor_default

  }

  if(max_cor > 1 || max_cor < 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is outside its valid range (>=0.1 to <=1), resetting it to '", max_cor_default, "'."
      )

    }

    max_cor <- max_cor_default

  }

  attr(
    x = max_cor,
    which = "validated"
  ) <- TRUE

  max_cor

}
