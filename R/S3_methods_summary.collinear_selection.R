#' Summary of \code{class.collinear_selection}
#'
#' @param object (sub-list in output of [collinear()] or [collinear_auto()]) Object to summarize. Default: NULL
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

  out <- object$selection

  # response ----
  if(!is.null(object$response)){

    msg <- paste0(
      "response: ",
      object$response
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
  if(length(out) > 1){

    cat(
      "+ selection:\n  -",
      paste(
        out,
        collapse = "\n  - "
      )
    )

  } else {

    cat(
      "- selection:\n  -", out
    )

  }

  cat("\n")

  out

}
