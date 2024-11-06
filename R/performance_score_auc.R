
#' Area Under the Curve of Binomial Observations vs Probabilistic Model Predictions
#'
#' @description
#' Internal function to compute the AUC of binomial models within [preference_order()]. As it is build for speed, this function does not check the inputs.
#'
#'
#' @param o (required, binomial vector) Binomial response with values 0 and 1. Default: NULL
#' @param p (required, numeric vector) Predictions of binomial model. Default: NULL
#'
#' @return numeric: Area Under the ROC Curve
#' @export
#' @family modelling_tools
#' @autoglobal
performance_score_auc <- function(
    o = NULL,
    p = NULL
){

  if(is.null(o)){
    stop(
      "collinear::performance_score_auc(): argument 'o' cannot be NULL.",
      call. = FALSE
    )
  }


  if(is.null(p)){
    stop(
      "collinear::performance_score_auc(): argument 'p' cannot be NULL.",
      call. = FALSE
    )
  }

  #predicted values of the ones and the zeroes
  ones <- p[o == 1]
  zeros <- p[o == 0]

  #lengths of each vector
  ones.n <- length(ones)
  zeros.n <- length(zeros)

  if(sum(c(ones.n, zeros.n)) == 0){
    stop(
      "collinear::performance_score_auc(): argument 'o' must be a binomial vector of 0s and 1s.",
      call. = FALSE
    )
  }

  #curve computation
  curve <- sum(
    rank(c(ones, zeros))[1:ones.n]
  ) - (ones.n * (ones.n + 1) / 2)

  #area under the curve
  curve / (zeros.n * ones.n)

}
