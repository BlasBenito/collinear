#' Print Method for Class \code{collinear_output}
#'
#' Prints an obect of the class [collinear_output] produced by [build.collinear_output()].
#'
#' @param x (required, list) Object of class \code{collinear_output} resulting from [build.collinear_output()]. Default: NULL
#' @param n (optional, integer) Maximum number of vector elements to print. Defaults: 5.
#' @param ... (optional) Additional arguments (currently ignored).
#'
#' @method print collinear_output
#' @autoglobal
#' @export
print.collinear_output <- function(
    x = NULL,
    n = 5,
    ...
){

  # response ----
  if(!is.null(x$response)){

    if(length(x$response) == 1){

      cat(
        " - response:",
        x$response,
        fill = TRUE
      )

    } else {

      cat(
        " + response:\n  - ",
        paste(x$response, collapse = "\n  -"),
        fill = TRUE
      )


    }



  }


  # selection ----
  if(!is.null(x$selection)){

    if(length(x$selection) <= n){

      cat(
        " + selection:\n   -",
        paste(
          x$selection,
          collapse = "\n   - "
        )
      )

    } else {

      cat(
        " + selection:\n   -",
        paste(
          x$selection[1:n],
          collapse = "\n   - "
        )
      )

      omitted <- length(x$selection) - n

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

    cat(" - df:", fill = TRUE)

    cat("   - rows:", nrow(x$df), fill = TRUE)

    cat("   - cols:", ncol(x$df))

    cat("\n")

  }


  # predictors ----
  if(!is.null(x$predictors)){

    if(length(x$predictors) <= 5){

      cat(
        " + predictors:\n   -",
        paste(
          x$predictors,
          collapse = "\n   - "
        )
      )

    } else {

      cat(
        " + predictors:\n   -",
        paste(
          x$predictors[1:n],
          collapse = "\n   - "
        )
      )

      omitted <- length(x$predictors) - n

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

  # preference ----
  if(!is.null(x$preference)){

    cat(" + preference order:", fill = TRUE)
    cat("   - df:", fill = TRUE)

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


  # timestamp ----

  if(!is.null(x$timestamp)){
    cat(" - timestamp:", as.character(format(x$timestamp, "%Y-%m-%d %H:%M:%S")), "\n")
  }

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

