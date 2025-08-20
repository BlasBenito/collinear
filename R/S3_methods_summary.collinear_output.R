#' Summary Method for Class \code{collinear_output}
#'
#' Prints the summary of an obect of the class [collinear_output] produced by [build.collinear_output()].
#'
#' @param x (required, list) Object of class \code{collinear_output} resulting from [build.collinear_output()]. Default: NULL
#'
#' @method summary collinear_output
#' @autoglobal
#' @export
summary.collinear_output <- function(
    x = NULL
){

  # response ----
  if(!is.null(x$response)){

    cat(
      " - response:",
      x$response,
      fill = TRUE
    )

  }


  # selection ----
  if(!is.null(x$selection)){

    cat(
      " + selection:\n   -",
      paste(
        x$selection,
        collapse = "\n   - "
      )
    )

    cat("\n")

  }

  invisible(x)
}
