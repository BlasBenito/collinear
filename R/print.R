#' Print Method for Class \code{collinear_output}
#'
#' Prints a summary of objects of the class [collinear_output] produced by [collinear()] when the argument \code{response} is \code{NULL} or has length one.
#'
#' @param x (required, list) Object of class \code{collinear_output} resulting from [collinear()]. Default: NULL
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

  # response
  if(is.null(x$response)){

    cat(
      " - response: NULL",
      fill = TRUE
    )

  } else {

    cat(
      " - response:",
      x$response,
      fill = TRUE
    )

  }


  # Predictors
  if(is.null(x$predictors)){

    cat(
      " - predictors: NULL",
      fill = TRUE
    )

  } else {

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

  # Selection
  if(is.null(x$selection)){

    cat(
      " - selection: NULL",
      fill = TRUE
      )

  } else {

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


  # Data frame summary
  if(is.null(x$df)){

    cat(
      " - df: NULL",
      fill = TRUE
    )

  } else {

    cat(" - df:", fill = TRUE)

    cat("   - rows:", nrow(x$df), fill = TRUE)

    cat("   - cols:", ncol(x$df))

    cat("\n")

  }

  # Arguments


  cat(" + arguments:", fill = TRUE)

  ## Encoding method
  if(is.null(x$arguments$encoding_method)){

    cat(
      "   - encoding_method: NULL",
      fill = TRUE
    )

  } else {

    cat(
      "   - encoding_method:",
      x$arguments$encoding_method,
      fill = TRUE
    )

  }

  ## Preference order
  if(is.null(x$arguments$preference_order)){

    cat(
      "   - preference_order: NULL",
      fill = TRUE
    )

  } else {

    if(length(x$arguments$preference_order) <= n){

      cat(
        "   + preference_order:\n     -",
        paste(
          x$arguments$preference_order$predictor,
          collapse = "\n     - "
        )
      )

    } else {

      cat(
        "   + preference_order:\n     -",
        paste(
          x$arguments$preference_order[1:n],
          collapse = "\n     - "
        )
      )

      omitted <- length(x$arguments$preference_order) - n

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
  if(is.null(x$arguments$f)){

    cat(
      "   - f: NULL",
      fill = TRUE
    )

  } else {

    f_df <- f_functions()

    if(x$arguments$f %in% f_df$name){

      f_df <- f_df[f_df$name == x$arguments$f, ]

      cat(
        "   + f:",
        fill = TRUE
      )

      cat("     - name:", x$arguments$f, fill = TRUE)

      cat("     - expression:", f_df$expression, fill = TRUE)

      cat("     - metric:", f_df$preference_metric, fill = TRUE)


    } else {

      cat(
        "   - f:",
        x$arguments$f,
        fill = TRUE
      )

    }

  }


  #max_cor
  if(is.null(x$arguments$max_cor)){

    cat(
      "   - max_cor: NULL",
      fill = TRUE
    )

  } else {

    cat(
      "   - max_cor:",
      round(x$arguments$max_cor, 2),
      fill = TRUE
    )

  }

  #max_cor
  if(is.null(x$arguments$max_cor)){

    cat(
      "   - max_vif: NULL",
      fill = TRUE
    )

  } else {

    cat(
      "   - max_vif:",
      round(x$arguments$max_vif, 1),
      fill = TRUE
    )

  }

  #formulas
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

    cat(" - timestamp:", as.character(format(x$timestamp, "%Y-%m-%d %H:%M:%S")), "\n")


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

