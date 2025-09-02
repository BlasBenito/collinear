#' @title Case Weights for Unbalanced Binomial or Categorical Responses
#'
#' @param x (required, integer, character, or factor vector) Binomial, categorical, or factor response variable. Default: NULL
#' @return numeric vector: case weights
#' @examples
#'  #numeric vector
#'  y <- case_weights(
#'    x = c(0, 0, 1, 1)
#'    )
#'
#'  #logical vector
#'  y <- case_weights(
#'    x = c(TRUE, TRUE, FALSE, FALSE)
#'    )
#'
#'  #character vector
#'  y <- case_weights(
#'    x = c("a", "a", "b", "c")
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

  if(is.logical(x)){
    x <- as.numeric(x)
  }

  if(is.numeric(x) && any(!is.finite(x))){
    stop(
      "collinear::case_weights(): NA, Inf, -Inf, or NaN values are not allowed when 'x' is numeric or logical.",
      call. = FALSE
    )
  }

  #convert NA, Inf, -Inf, or NaN to character
  if(is.character(x)){
    x <- paste(x)
  }

  #weights per class as inverse of the counts
  weights <- 1 / table(as.factor(x))

  #full vector of weights
  out <- weights[as.character(x)]

  #normalize to one
  out <- out / sum(out)

  if(length(out) != length(x)){
    stop(
      "collinear::case_weights(): something went wrong when computing case weights, the length of the input is ",
      length(x),
      ", but the length of the output is ",
      length(out),
      ".",
      call. = FALSE
    )
  }

  out

}
