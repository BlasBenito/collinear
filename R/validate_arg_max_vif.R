#' Validate Argument \code{max_vif}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#'
#' @return numeric or NULL
#' @autoglobal
#' @export
#' @family data_validation
#' @examples
#' max_vif <- validate_arg_max_vif(
#'   max_vif = 11, #wrong value
#'   function_name = "f()",
#'   quiet = FALSE
#' )
#'
#' max_vif
#' attributes(max_vif)$validated
validate_arg_max_vif <- function(
    max_vif = NULL,
    function_name = NULL,
    quiet = FALSE
    ){

  if(is.null(function_name)){
    function_name <- "collinear::validate_arg_max_vif()"
  }

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

  max_vif <- as.numeric(max_vif)

  if(length(max_vif) > 1){

    max_vif <- max_vif[1]

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' has more than one value, using the first one (", max_vif, ")."
      )

    }

  }

  if(!is.numeric(max_vif)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is non-numeric, resetting it to its default value (", max_vif_default, ")."
        )

    }

    max_vif <- max_vif_default

  }

  if(max_vif > 10 || max_vif < 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is outside its valid range (>=1 to <=10), resetting it to ", max_vif_default, "."
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
