#' Print Method for Class \code{collinear_arguments}
#'
#' Prints an object of the class \code{collinear_arguments} produced by [collinear()].
#'
#' @param x (required, list) Object of class \code{collinear_arguments}. Default: NULL
#' @param n (optional, integer) Maximum number of vector elements to print. Default: 5.
#'
#' @method print collinear_arguments
#' @autoglobal
#' @export
print.collinear_arguments <- function(
    x = NULL,
    n = 5
){

  # df ----
  if(!is.null(x$df)){

    cat(" - df:", fill = TRUE)

    cat("   - rows:", nrow(x$df), fill = TRUE)

    cat("   - cols:", ncol(x$df))

    cat("\n")

  }


  cat(" + arguments:", fill = TRUE)

  ## Encoding method
  if(!is.null(x$encoding_method)){

    cat(
      "   - encoding_method:",
      x$encoding_method,
      fill = TRUE
    )

  }

  ## Preference order
  #TODO: does not support preference_order lists yet
  if(!is.null(x$preference_order)){

    if(is.data.frame(x$preference_order)){

      preference_order.vector <- x$preference_order$predictor

      cat(
        "   + preference_order:\n     -",
        paste(
          preference_order.vector[1:n],
          collapse = "\n     - "
        )
      )

    if(length(preference_order.vector) > n){

      omitted <- length(preference_order.vector) - n

      cat(
        paste0(
          "\n     - ... (",
          omitted,
          " ommited)"
        )
      )

    cat("\n")

    }

    }

  }

  #f
  if(!is.null(x$f)){

    cat(
      "   - f:",
      x$f,
      fill = TRUE
    )

  }


  #max_cor
  if(!is.null(x$max_cor)){

    cat(
      "   - max_cor:",
      round(x$max_cor, 2),
      fill = TRUE
    )

  }

  #max_cor
  if(!is.null(x$max_vif)){

    cat(
      "   - max_vif:",
      round(x$max_vif, 1),
      fill = TRUE
    )

  }


}
