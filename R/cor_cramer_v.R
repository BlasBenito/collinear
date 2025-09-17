#' Bias Corrected Cramer's V
#'
#' @description
#'
#'
#' Cramer's V extends the chi-squared test to quantify how strongly the
#' categories of two variables co-occur. The value ranges from 0 to 1,
#' where 0 indicates no association and 1 indicates perfect association.
#'
#' When both variables are binary, Cramer's V is mathematically identical to the absolute Pearson correlation. With more than two categories the relationship diverges: as the number of categories increases, Pearson correlation (which depends on numeric coding) and Cramer's V (which depends only on the contingency table) become progressively less comparable.
#'
#' If you intend to combine these measures in a multicollinearity analysis, interpret them with care. It is often preferable to convert non-numeric variables to numeric form (for example, via target encoding) before assessing multicollinearity.
#'
#' @param x (required; vector) Values of a categorical variable (character or vector). Converted to character if numeric or logical. Default: NULL
#' @param y (required; vector) Values of a categorical variable (character or vector). Converted to character if numeric or logical. Default: NULL
#' @param check_input (required; logical) If FALSE, disables data checking for a slightly faster execution. Default: TRUE
#' @inheritParams collinear
#'
#' @return numeric: Cramer's V
#'
#' @examples
#'
#' # perfect one-to-one association
#' cor_cramer_v(
#'   x = c("a", "a", "b", "c"),
#'   y = c("a", "a", "b", "c")
#' )
#'
#' # still perfect: labels differ but mapping is unique
#' cor_cramer_v(
#'   x = c("a", "a", "b", "c"),
#'   y = c("a", "a", "b", "d")
#' )
#'
#' # high but < 1: mostly aligned, one category of y repeats
#' cor_cramer_v(
#'   x = c("a", "a", "b", "c"),
#'   y = c("a", "a", "b", "b")
#' )
#'
#' # appears similar by position, but no association by distribution
#' # (x = "a" mixes with y = "a" and "b")
#' cor_cramer_v(
#'   x = c("a", "a", "a", "c"),
#'   y = c("a", "a", "b", "b")
#' )
#'
#' # numeric inputs are coerced to character internally
#' cor_cramer_v(
#'   x = c(1, 1, 2, 3),
#'   y = c(1, 1, 2, 2)
#' )
#'
#' # logical inputs are also coerced to character
#' cor_cramer_v(
#'   x = c(TRUE, TRUE, FALSE, FALSE),
#'   y = c(TRUE, TRUE, FALSE, FALSE)
#' )
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
    check_input = TRUE,
    ...
) {

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_cramer_v()",
    ... = ...
  )

  #data checks
  if(check_input == TRUE){

    # Check if 'x' is not NULL
    if(is.null(x)){
      stop(
        "\n",
        function_name,
        ": argument 'x' must not be NULL.",
        call. = FALSE
      )
    }

    # Check if 'y' is not NULL
    if(is.null(y)){
      stop(
        "\n",
        function_name,
        ": argument 'y' must not be NULL.",
        call. = FALSE
      )
    }

    #x and y have the same length
    if(length(x) != length(y)){
      stop(
        "\n",
        function_name,
        ": arguments 'x' and 'y' must have the same length.",
        call. = FALSE
        )
    }

  }

  #to data frame to remove NA
  xy.df <- data.frame(
    x = as.character(x),
    y = as.character(y)
  ) |>
    stats::na.omit()

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

  xy.chi <- tryCatch(
    {
      suppressWarnings(
        stats::chisq.test(
          xy.table,
          simulate.p.value = TRUE
        )$statistic
      )
    },
    error = function(e) {
      stop(
        function_name <- validate_arg_function_name(
          default_name = "stats::chisq.test()",
          function_name = function_name
        ),
        ": chi-squared test failed: ",
        e$message,
        call. = FALSE
      )
    }
  )

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
