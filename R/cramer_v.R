#' Cramer's V for categorical variables
#'
#' @description
#'
#' The `cramer_v()` function calculates Cramer's V, a measure of association between two categorical variables.
#'
#' Cramer's V is an extension of the chi-squared test to measure the strength of association between two categorical variables. Provides values between 0 and 1, where 0 indicates no association, and 1 indicates a perfect association. In essence, Cramer's V assesses the co-occurrence of the categories of two variables to quantify how strongly these variables are related.
#'
#' @param x (required; character vector) character vector representing a categorical variable.  Default: NULL
#' @param y (required; character vector) character vector representing a categorical variable. Must have the same length as 'x'. Default: NULL
#' @param check_input (required; logical) If FALSE, disables data checking for a slightly faster execution. Default: TRUE
#'
#' @return Numeric, value of Cramer's V
#'
#' @examples
#' if(interactive()){
#' data(vi)
#' cramer_v(
#'   x = vi$primary_productivity,
#'   y = vi$dominant_landcover
#'   )
#' }
#' @autoglobal
#' @export
cramer_v <- function(
    x = NULL,
    y = NULL,
    check_input = TRUE
) {

  #data checks
  if(check_input == TRUE){

    # Check if 'x' and 'y' have the same length
    if(length(x) != length(y)){
      stop("arguments 'x' and 'y' must have the same length.")
    }

    # Check if 'x' is not NULL
    if(is.null(x)){
      stop("argument 'x' must not be NULL.")
    }

    # Check if 'y' is not NULL
    if(is.null(y)){
      stop("argument 'y' must not be NULL.")
    }

    # Check if 'x' is a character vector
    if(is.numeric(x)){
      stop("Argument 'x' must be of class 'character' or 'factor', but it is 'numeric'.")
    }

    # Check if 'y' is a character vector
    if(is.numeric(y)){
      stop("argument 'y' must be of class 'character' or 'factor', but it is 'numeric'.")
    }

  }

  # contingency table of 'x' and 'y'
  xy.table <- table(
    as.character(x),
    as.character(y)
    )

  # chi-squared test with Monte Carlo simulation
  #for p-value estimation
  xy.chi <- stats::chisq.test(
    xy.table,
    simulate.p.value = TRUE
  )$statistic |>
    suppressWarnings()

  # number of cases
  n <- sum(xy.table)

  # minimum number of categories
  min_dim <- min(dim(xy.table))

  # Cramer's V computation
  v <- sqrt(xy.chi / (n * (min_dim - 1)))

  #remove names from the output
  names(v) <- NULL

  v

}
