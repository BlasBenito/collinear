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
#' @examples
#'   performance_score_r2(
#'     o = c(1, 1, 1, 0.5, 0.5, 0, 0),
#'     p = c(1, 0.8, 0.7, 0.6, 0.5, 0.1, 0)
#'   )
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
