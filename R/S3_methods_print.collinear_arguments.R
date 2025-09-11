#' Print \code{class.collinear_arguments}
#' @param x (required, output of [class.collinear_arguments()]) Object to print. Default: NULL
#' @param n (optional, integer) Maximum printed vector length. Default: 5.
#' @param ... Ignored, kept for consistency with generic.
#' @method print collinear_arguments
#' @family S3_methods
#' @autoglobal
#' @export
print.collinear_arguments <- function(
    x = NULL,
    n = 5,
    ...
){


  # timestamp ----
  if(!is.null(x$timestamp)){

    cat("Timestamp\n")
    cat("===================\n")

    cat(as.character(format(x$timestamp, "%Y-%m-%d %H:%M:%S")), "\n")

    cat("\n")

  }

  cat("Validated Arguments\n")
  cat("===================\n")

  # df ----
  if(!is.null(x$df)){

    cat(" + df:", fill = TRUE)

    cat("   - rows:", nrow(x$df), fill = TRUE)

    cat("   - cols:", ncol(x$df))

    cat("\n")

  }

  # response ----
  if(!is.null(x$response)){

    response <- stats::na.omit(x$response[1:n])

    omitted <- length(x$response) - n

    symbol <- ifelse(
      test = length(response) == 1,
      yes = " -",
      no = " +"
    )

    cat(
      symbol,
      "response:\n   -",
      paste(
        response,
        collapse = "\n   - "
      )
    )

    if(omitted > 0){

      cat(
        paste0(
          "\n   - ... (",
          omitted,
          " ommited)"
        )
      )

    }

    cat("\n")

  }

  # predictors ----
  if(!is.null(x$predictors)){

    predictors <- stats::na.omit(x$predictors[1:n])

    omitted <- length(x$predictors) - n

    symbol <- ifelse(
      test = length(predictors) == 1,
      yes = " -",
      no = " +"
    )

    cat(
      symbol,
      "predictors:\n   -",
      paste(
        predictors,
        collapse = "\n   - "
      )
    )

    if(omitted > 0){

      cat(
        paste0(
          "\n   - ... (",
          omitted,
          " ommited)"
        )
      )

    }

    cat("\n")

  }

  ## Encoding method
  if(!is.null(x$encoding_method)){

    cat(
      " - encoding_method:",
      x$encoding_method,
      fill = TRUE
    )

  }

  ## Preference order
  #TODO: does not support preference_order lists yet
  if(!is.null(x$preference_order)){

    if(is.data.frame(x$preference_order)){

      preference_order.vector <- x$preference_order$predictor

    } else if(is.character(x$preference_order)){

      preference_order.vector <- x$preference_order

    }

    preference_order.length <- length(preference_order.vector)

    preference_order.vector <- stats::na.omit(preference_order.vector[1:n])

    cat(
      " + preference_order:\n     -",
      paste(
        preference_order.vector,
        collapse = "\n     - "
      )
    )

    if(length(preference_order.vector) > n){

      omitted <- preference_order.length - n

      cat(
        paste0(
          "\n     - ... (",
          omitted,
          " ommited)"
        )
      )

    }

    cat("\n")

  }

  #f
  if(!is.null(x$f_name)){

    cat(
      " - f:",
      x$f_name,
      fill = TRUE
    )

  }


  #max_cor
  if(!is.null(x$max_cor)){

    cat(
      " - max_cor:",
      round(x$max_cor, 2),
      fill = TRUE
    )

  }

  #max_cor
  if(!is.null(x$max_vif)){

    cat(
      " - max_vif:",
      round(x$max_vif, 1),
      fill = TRUE
    )

  }

  cat("\n")


}
