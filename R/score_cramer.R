#' Compute Cramer's V between categorical observations and predictions
#'
#' @description
#' Internal function to compute the Cramer's V of categorical observations versus categorical model predictions. Please read the help file of [cor_cramer()] for further details.
#'
#'
#' @param o (required; character vector) categorical observations. Default: NULL
#' @param p (required; character vector) categorical predictions. Default: NULL
#' @inheritParams collinear
#' @return numeric: Cramer's V
#' @export
#' @autoglobal
#' @family modelling_tools
#' @examples
#' score_cramer(
#'  o = c("a", "a", "b", "c", "c"),
#'  p = c("a", "b", "b", "c", "c")
#'  )
#'
score_cramer <- function(
  o = NULL,
  p = NULL,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::score_cramer()",
    ... = ...
  )

  out <- tryCatch(
    {
      cor_cramer(
        x = o,
        y = p,
        check_input = FALSE,
        function_name = function_name
      )
    },
    error = function(e) {
      stop(
        "\n",
        function_name,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  out
}
