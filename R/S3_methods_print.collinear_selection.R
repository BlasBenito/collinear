#' Print \code{class.collinear_selection}
#' @param x (required, output of [class.collinear_selection()]) Object to print. Default: NULL
#' @param n (optional, integer) Maximum printed vector length. Default: 5.
#' @param ... Ignored, kept for consistency with generic.
#' @method print collinear_selection
#' @family S3_methods
#' @autoglobal
#' @export
print.collinear_selection <- function(
    x = NULL,
    n = 5,
    ...
){

  # response ----
  if(!is.null(x$response)){

    msg <- paste0(
      "response: ",
      x$response
      )

    msg_length <- nchar(msg)

    underline <- paste0(rep(x = "-", times = nchar(msg)), collapse = "")

      cat(
        " -",
        msg,
        fill = TRUE
      )

      cat("  ", underline)

      cat("\n")

  }


  # selection ----
  if(!is.null(x$selection)){

    cat("\n")

    selection <- stats::na.omit(x$selection[1:n])

    omitted <- length(x$selection) - n

    symbol <- ifelse(
      test = length(selection) == 1,
      yes = " -",
      no = " +"
    )

    cat(
      symbol,
      "selection:\n   -",
      paste(
        selection,
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


  # formulas ----
  if(!is.null(x$formulas)){

    cat("\n")

    if(length(x$formulas) > 1){
      cat(" + formulas:", fill = TRUE)
    } else {
      cat(" - formula:", fill = TRUE)
    }

    for(i in names(x$formulas)){
      cat(paste0("   - ", i, ": ", short_formula(f = x$formulas[[i]], n = n)), "\n")
    }

  }

  # df ----
  if(!is.null(x$df)){

    cat("\n")

    cat(" + df:", fill = TRUE)

    cat("   - rows:", nrow(x$df), fill = TRUE)

    cat("   - cols:", ncol(x$df))

    cat("\n")

  }

  # preference ----
  if(!is.null(x$preference)){

    cat("\n")

    cat(" + preference order:", fill = TRUE)
    cat("   + df:", fill = TRUE)

    cat("     - rows:", nrow(x$preference$df), fill = TRUE)

    cat("     - cols:", ncol(x$preference$df))

    cat("\n")

    if(!is.null(x$preference$f)){

      cat("   + f:", fill = TRUE)

      cat("     - function:", x$preference$f$name, fill = TRUE)

      cat("     - expression:", x$preference$f$expression, fill = TRUE)

      cat("     - metric:", x$preference$f$metric)

      cat("\n")

    }

  }

  cat("\n\n")

  invisible(x)
}


#' Shorten Formula for Concise Printing
#'
#' Produces a compact string representation of a formula.
#'
#' @param f (required, model formula) Model formula to shorten. Default: NULL
#'
#' @return character string
#' @autoglobal
#' @keywords internal
#' Shorten Formula for Concise Printing
#'
#' Produces a compact string representation of a formula.
#'
#' @param f (required, model formula) Model formula to shorten. Default: NULL
#' @param n (required, integer) Number of terms to print. Default: 5
#'
#' @return character string
#' @autoglobal
#' @keywords internal
short_formula <- function(f = NULL, n = 5){

  terms <- attr(terms(f), "term.labels")
  nterms <- length(terms)

  n <- max(n, 1)
  n <- min(n, nterms)

  #adding one term if needed
  if(nterms == (n + 1)){
    n <- nterms
  }

  lhs <- deparse(f[[2]])

  rhs <- paste0(terms[1:n], collapse = " + ")

  f_string <- paste(lhs, rhs, sep = " ~ ")

  notice <- if(n < nterms){
    paste0(
      "+ ... (",
      nterms - n,
      " terms omitted)"
    )
  } else {
    NULL
  }

  paste(f_string, notice)

}

