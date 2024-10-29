#' Bias Corrected Cramer's V
#'
#' @description
#'
#' Computes bias-corrected Cramer's V (extension of the chi-squared test), a measure of association between two categorical variables. Results are in the range 0-1, where 0 indicates no association, and 1 indicates a perfect association.
#'
#' In essence, Cramer's V assesses the co-occurrence of the categories of two variables to quantify how strongly these variables are related.
#'
#' Even when its range is between 0 and 1, Cramer's V values are not directly comparable to R-squared values, and as such, a multicollinearity analysis containing both types of values must be assessed with care. It is probably preferable to convert non-numeric variables to numeric using target encoding rather before a multicollinearity analysis.
#'
#' @param x (required; character vector) character vector representing a categorical variable.  Default: NULL
#' @param y (required; character vector) character vector representing a categorical variable. Must have the same length as 'x'. Default: NULL
#' @param check_input (required; logical) If FALSE, disables data checking for a slightly faster execution. Default: TRUE
#'
#' @return numeric: Cramer's V
#'
#' @examples
#'
#' #loading example data
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #computing Cramer's V for two categorical predictors
#' v <- cor_cramer_v(
#'   x = vi$soil_type,
#'   y = vi$koppen_zone
#'   )
#'
#' v
#'
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @references
#' \itemize{
#'  \item Cramér, H. (1946). Mathematical Methods of Statistics. Princeton: Princeton University Press, page 282 (Chapter 21. The two-dimensional case). ISBN 0-691-08004-6
#' }
#' @export
cor_cramer_v <- function(
    x = NULL,
    y = NULL,
    check_input = TRUE
) {

  #data checks
  if(check_input == TRUE){

    # Check if 'x' and 'y' have the same length
    if(length(x) != length(y)){
      stop(
        "collinear::cor_cramer_v(): arguments 'x' and 'y' must have the same length.",
        call. = FALSE
        )
    }

    # Check if 'x' is not NULL
    if(is.null(x)){
      stop(
        "collinear::cor_cramer_v(): argument 'x' must not be NULL.",
        call. = FALSE
      )
    }

    # Check if 'y' is not NULL
    if(is.null(y)){
      stop(
        "collinear::cor_cramer_v(): argument 'y' must not be NULL.",
        call. = FALSE
      )
    }

    # Check if 'x' is a character vector
    if(is.numeric(x)){
      stop(
        "collinear::cor_cramer_v(): argument 'x' must be of class 'character' or 'factor', but it is 'numeric'.",
        call. = FALSE
      )
    }

    # Check if 'y' is a character vector
    if(is.numeric(y)){
      stop(
        "collinear::cor_cramer_v(): argument 'y' must be of class 'character' or 'factor', but it is 'numeric'.",
        call. = FALSE
      )
    }

  }

  #to data frame to remove NA
  xy.df <- data.frame(
    x = as.character(x),
    y = as.character(y)
  ) |>
    na.omit()

  # contingency table of 'x' and 'y'
  xy.table <- table(
    xy.df$x,
    xy.df$y
  )

  # chi-squared test with Monte Carlo simulation
  #for p-value estimation
  xy.chi <- stats::chisq.test(
    xy.table,
    simulate.p.value = TRUE
  )$statistic |>
    suppressWarnings()

  #columns of xy.table
  xy.table.cols <- ncol(xy.table)

  #rows of xy.table
  xy.table.rows <- nrow(xy.table)

  #total sample size
  xy.table.sum <- sum(xy.table)

  #bias corrected Cramer's V
  v <- sqrt(
    max(
      c(
        0,
        (xy.chi /  xy.table.sum) - ((xy.table.cols - 1)*(xy.table.rows - 1)) /
          (xy.table.sum - 1)
      )
    ) /
      min(
        c(
          (
            xy.table.cols -
              ((xy.table.cols - 1)^2 /
                 (xy.table.sum - 1))
          ) - 1,
          (xy.table.rows -
             ((xy.table.rows - 1)^2 /
                (xy.table.sum - 1))
          ) - 1
        )
      )
  )

  #Cramer's V with no bias correction
  #kept here for reference
  # min_dim <- min(dim(xy.table))
  # v <- sqrt(xy.chi / (xy.table.sum * (min_dim - 1)))

  #remove names from the output
  names(v) <- NULL

  v

}
