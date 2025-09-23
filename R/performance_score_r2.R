#' Pearson's R-squared of Observations vs Predictions
#'
#' @description
#' Internal function to compute the R-squared of observations versus predictions via [stats::cor()]. Used within [f_r2_glm_gaussian()], [f_r2_glm_gaussian_poly2()], [f_r2_gam_gaussian()], [f_r2_rpart()], [f_r2_rf()], [f_r2_glm_poisson()], [f_r2_glm_poisson_poly2()], and [f_r2_gam_poisson()].
#'
#' @param o (required, numeric vector) Observations. Default: NULL
#' @param p (required, numeric vector) Predictions. Default: NULL
#' @inheritParams collinear
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
    p = NULL,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::performance_score_r2()",
    ... = ...
  )

  tryCatch(
    {
      stats::cor(
        x = p,
        y = o,
        use = "complete.obs",
        method = "pearson"
      )^2
    },
    error = function(e) {
      stop(
        "\n",
        function_name,
        ": ", conditionMessage(e), call. = FALSE)
    }
  )


}
