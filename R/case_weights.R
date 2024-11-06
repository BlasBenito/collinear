#' @title Case Weights for Unbalanced Binomial or Categorical Responses
#'
#' @param x (required, integer, character, or factor vector) binomial, categorical, or factor response variable. Default: `NULL`
#' @return numeric vector: case weights
#' @examples
#'  case_weights(
#'    x = c(0, 0, 0, 1, 1)
#'    )
#'
#'  case_weights(
#'    x = c("a", "a", "b", "b", "c")
#'    )
#' @family modelling_tools
#' @autoglobal
#' @export
case_weights <- function(
    x = NULL
){

  if(is.null(x)){
    stop(
      "collinear::case_weights(): argument 'x' cannot be NULL.",
      call. = FALSE
    )
  }

  # weights per class
  # as inverse of the counts
  weights <- 1 / table(as.factor(x))

  # full vector of weights
  weights[as.character(x)]

}
