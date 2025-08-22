#' Summary Method for Class \code{collinear_selection}
#'
#' @param x (required, list) Object of class \code{collinear_selection} resulting from [build.collinear_selection()]. Default: NULL
#'
#' @method summary collinear_selection
#' @autoglobal
#' @export
summary.collinear_selection <- function(
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
