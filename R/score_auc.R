#' Compute area under the ROC curve between binomial observations and probabilistic predictions
#'
#' @description
#' Internal function to compute the AUC of binomial models within [preference_order()]. Used within [f_binomial_glm()], [f_binomial_gam()], and [f_binomial_rf()]. This function is build for speed and it does not check the inputs.
#'
#' @param o (required, numeric vector) Binomial observations (values 0 and 1). Default: NULL
#' @param p (required, numeric vector) Prediction of binomial model in the range 0-1. Default: NULL
#' @inheritParams collinear
#'
#' @return numeric: Area Under the ROC Curve
#' @export
#' @family modelling_tools
#' @autoglobal
#' @examples
#'   score_auc(
#'     o = c(1, 1, 1, 1, 0, 0, 0),
#'     p = c(1, 0.8, 0.7, 0.6, 0.5, 0.6, 0.7)
#'   )
#'
score_auc <- function(
  o = NULL,
  p = NULL,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::score_auc()",
    ... = ...
  )

  if (is.null(o)) {
    stop(
      "\n",
      function_name,
      ": argument 'o' cannot be NULL.",
      call. = FALSE
    )
  }

  if (is.null(p)) {
    stop(
      "\n",
      function_name,
      ": argument 'p' cannot be NULL.",
      call. = FALSE
    )
  }

  #predicted values of the ones and the zeroes
  ones <- p[o == 1]
  zeros <- p[o == 0]

  if (sum(ones) == 0) {
    stop(
      "\n",
      function_name,
      ": argument 'o' must be a binomial vector of 0s and 1s.",
      call. = FALSE
    )
  }

  #lengths of each vector
  ones.n <- length(ones)
  zeros.n <- length(zeros)

  #curve computation
  curve <- sum(
    rank(c(ones, zeros))[1:ones.n]
  ) -
    (ones.n * (ones.n + 1) / 2)

  #area under the curve
  out <- curve / (zeros.n * ones.n)

  out
}
