#' Cramer's V of Observations vs Predictions
#'
#' @description
#' Internal function to compute the Cramer's V of categorical observations versus categorical model predictions.
#'
#'
#' @param o (required, numeric vector) Response values. Default: NULL
#' @param p (required, numeric vector) Model predictions. Default: NULL
#'
#' @return numeric: Cramer's V
#' @export
#' @autoglobal
#' @family modelling_tools
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
