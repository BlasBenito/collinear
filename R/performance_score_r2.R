#' Pearson's R-squared of Observations vs Predictions
#'
#' @description
#' Internal function to compute the R-squared of observations versus model predictions.
#'
#'
#' @param o (required, numeric vector) Response values. Default: NULL
#' @param p (required, numeric vector) Model predictions. Default: NULL
#'
#' @return numeric: Pearson R-squared
#' @export
#' @autoglobal
#' @family modelling_tools
performance_score_r2 <- function(
    o = NULL,
    p = NULL
){

  stats::cor(
    x = p,
    y = o,
    use = "complete.obs",
    method = "pearson"
  )^2

}
