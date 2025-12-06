#' @title Generate sample weights for imbalanced responses
#' @description Computes case weights adding to one for response variables of these types:
#'
#' \itemize{
#'   \item numeric binomial (1 and 0).
#'   \item logical (TRUE and FALSE): converted to numeric internally.
#'   \item categorical (character or factor)
#' }
#'
#' Values NA, Inf, -Inf, and NaN are invalid for numeric and logical variables and will result in errors. For categorical variables, these are converted to their respective categories ("NA", "Inf", "-Inf", and "NaN") with their assigned case weights.
#'
#' All returned weights sum to one.
#'
#' @param x (required, integer, character, or factor vector) Values of a binomial, categorical, or factor variable. Default: NULL
#' @inheritParams collinear
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
  x = NULL,
  ...
) {
  function_name <- validate_arg_function_name(
    default_name = "collinear::case_weights()",
    ... = ...
  )

  if (is.null(x)) {
    stop(
      "\n",
      function_name,
      ": argument 'x' cannot be NULL.",
      call. = FALSE
    )
  }

  if (is.logical(x)) {
    x <- as.numeric(x)
  }

  if (is.numeric(x) && any(!is.finite(x))) {
    stop(
      "\n",
      function_name,
      ": NA, Inf, -Inf, or NaN values are not allowed when 'x' is numeric or logical.",
      call. = FALSE
    )
  }

  #convert NA, Inf, -Inf, or NaN to character
  if (is.character(x)) {
    x <- paste(x)
  }

  #weights per class as inverse of the counts
  weights <- 1 / table(as.factor(x))

  #full vector of weights
  out <- weights[as.character(x)]

  #normalize to one
  out <- out / sum(out)

  if (length(out) != length(x)) {
    stop(
      "\n",
      function_name,
      ": something went wrong when computing case weights, the input length is ",
      length(x),
      ", but the output length is ",
      length(out),
      ".",
      call. = FALSE
    )
  }

  out
}
