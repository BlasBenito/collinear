#' Cramer's V of Observations vs Predictions
#'
#' @description
#' Internal function to compute the Cramer's V of categorical observations versus categorical model predictions.
#'
#'
#' @param o (required; character vector) character vector representing observations of a categorical variable. Default: NULL
#' @param p (required; character vector) character vector representing predictions of a categorical variable. Must have the same length as \code{o}. Default: NULL
#'
#' @return numeric: Cramer's V
#' @export
#' @autoglobal
#' @family modelling_tools
#' @examples
#' performance_score_v(
#'  o = c("a", "a", "b", "c", "c"),
#'  p = c("a", "b", "b", "c", "c")
#'  )
#'
performance_score_v <- function(
    o = NULL,
    p = NULL
){

  cor_cramer_v(
    x = o,
    y = p,
    check_input = FALSE
  )

}
