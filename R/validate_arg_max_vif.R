#' Validate Argument \code{max_vif}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return numeric or NULL
#' @autoglobal
#' @export
#' @family argument_validation
#' @examples
#' max_vif <- validate_arg_max_vif(
#'   max_vif = 11, #wrong value
#'   quiet = FALSE
#' )
#'
#' max_vif
#' attributes(max_vif)$validated
validate_arg_max_vif <- function(
    max_vif = NULL,
    quiet = FALSE,
    function_name = NULL
    ){

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_max_vif()",
    function_name = function_name
  )

  max_vif_default <- 5

  if(isTRUE(attr(x = max_vif, which = "validated"))){
    return(max_vif)
  }

  if(is.null(max_vif)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is NULL, skipping VIF filtering."
      )

    }

    return(NULL)

  }

  if(is.numeric(max_vif)){

    if(length(max_vif) > 1){

      max_vif <- max_vif[1]

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'max_vif' must be of length one, using value '", max_vif, "'."
        )

      }

    }

  } else {

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is non-numeric, resetting it to to '", max_vif_default, "'."
      )

    }

    max_vif <- max_vif_default

  }

  if(max_vif > 10 || max_vif < 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is outside its valid range (>=1 to <=10), resetting it to '", max_vif_default, "'."
      )

    }

    max_vif <- max_vif_default

  }

  attr(
    x = max_vif,
    which = "validated"
  ) <- TRUE

  max_vif

}
