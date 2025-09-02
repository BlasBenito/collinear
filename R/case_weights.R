#' @title Case Weights for Binomial, Logical, or Categorical Responses
#'
#' @description Computes case weights adding to one for response variables of these types:
#'
#' \itemize{
#'   \item numeric binomial (1 and 0).
#'   \item logical (TRUE and FALSE): converted to numeric internally.
#'   \item categorical
#' }
#'
#' Values NA, Inf, -Inf, and NaN are invalid for numeric and logical variables and will result in errors. For categorical variables, these, if present, are converted to their respective categories ("NA", "Inf", "-Inf", and "NaN") with their assigned case weights.
#'
#' All returned weights sum to one.
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
