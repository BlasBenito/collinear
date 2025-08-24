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

    msg <- paste0(
      "response: ",
      x$response
    )

    msg_length <- nchar(msg)

    underline <- paste0(rep(x = "-", times = nchar(msg)), collapse = "")

    cat(
      msg,
      fill = TRUE
    )

    cat(underline)

    cat("\n")

  }


  # selection ----
  if(!is.null(x$selection)){

    if(length(x$selection) > 1){

      cat(
        "+ selection:\n  -",
        paste(
          x$selection,
          collapse = "\n  - "
        )
      )

    } else {

      cat(
        "- selection:\n  -", x$selection
      )

    }

    cat("\n")

  }

  cat("\n")

  invisible(x)

}
