#' Summary of \code{class.collinear_selection}
#' @param object (required, output of [class.collinear_selection()]) Object to summarize. Default: NULL
#' @param ... Ignored, kept for consistency with generic.
#' @return list
#' TODO describe list structure
#' @method summary collinear_selection
#' @family S3_methods
#' @autoglobal
#' @export
summary.collinear_selection <- function(
    object = NULL,
    ...
){

  x <- list(
    response = object$response,
    selection = object$selection
  )

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

  class(x) <- c(class(x), "summary.collinear_selection")

  x

}
