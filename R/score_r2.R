#' Pearson's R-squared of Observations vs Predictions
#'
#' @description
#' Internal function to compute the R-squared of observations versus predictions via [stats::cor()]. Used within [f_numeric_glm()], [f_numeric_gam()], [f_numeric_rf()], [f_count_glm()], and [f_count_gam()].
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
#'   score_r2(
#'     o = c(1, 1, 1, 0.5, 0.5, 0, 0),
#'     p = c(1, 0.8, 0.7, 0.6, 0.5, 0.1, 0)
#'   )
score_r2 <- function(
  o = NULL,
  p = NULL,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::score_r2()",
    ... = ...
  )

  out <- tryCatch(
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
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  out
}
