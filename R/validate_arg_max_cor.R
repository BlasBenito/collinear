#' Validate Argument \code{max_cor}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return numeric or NULL
#' @autoglobal
#' @export
#' @family data_validation
#' @examples
#' max_cor <- validate_arg_max_cor(
#'   max_cor = 1.5, #wrong value
#'   function_name = "f()",
#'   quiet = FALSE
#' )
#'
#' max_cor
#' attributes(max_cor)$validated
validate_arg_max_cor <- function(
    max_cor = NULL,
    function_name = NULL,
    quiet = FALSE
    ){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_max_cor()",
    function_name = function_name
  )

  max_cor_default <- 0.75

  if(isTRUE(attr(x = max_cor, which = "validated_cor"))){
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

  if(length(max_cor) > 1){

    max_cor <- max_cor[1]

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' has more than one value, using the first one (", max_cor, ")."
      )

    }

  }

  if(!is.numeric(max_cor)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is non-numeric, resetting it to its default value (", max_cor_default, ")."
      )

    }

    max_cor <- max_cor_default

  }

  if(max_cor > 1 || max_cor < 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is outside its recommended range (>=0 to <=1), resetting it to its default value (", max_cor_default, ")."
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
