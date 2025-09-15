#' Variance Inflation Factors From Correlation Matrix
#'
#' @description Computes the Variance Inflation Factors of numeric variables from their correlation matrix in two steps:
#' \itemize{
#'   \item Applies [base::solve()] to transform the correlation matrix into a precision matrix, which is the inverse of the covariance matrix between all variables in \code{predictors}.
#'   \item Applies [base::diag()] to extract the diagonal of the precision matrix, which contains the variance of the regression of each predictor against all other predictors, also known as Variance Inflation Factor
#'}
#'
#' @inheritSection collinear Variance Inflation Factors
#'
#' @param m (required, matrix) Correlation matrix generated via [stats::cor()] or [cor_matrix()]. Must have named dimensions. Default: NULL
#' @inheritParams collinear
#'
#' @return named numeric vector
#' @family vif
#' @inherit vif_select references
#' @autoglobal
#' @export
#' @examples
#' data(vi, vi_predictors_numeric)
#'
#' m <- cor_matrix(
#'   df = vi[1:1000, ],
#'   predictors = vi_predictors_numeric[1:5]
#' )
#'
#' vif(m)
vif <- function(
    m = NULL,
    ...
    ){

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif()",
    ... = ...
  )

  if(inherits(x = m, what = "matrix") == FALSE){
    stop(
      "\n",
      function_name,
      ": argument 'm' must be a correlation matrix generated via 'stats::cor()' or 'collinear::cor_matrix()'.",
      call. = FALSE
    )
  }

  if(length(unique(dim(m))) > 1){
    stop(
      "\n",
      function_name,
      ": argument 'm' must be a square matrix.",
      call. = FALSE
    )
  }

  if(length(dimnames(m)[[1]]) != nrow(m)){
    stop(
      "\n",
      function_name,
      ": argument 'm' must have dimension names.",
      call. = FALSE
    )
  }

  out <- tryCatch(

    #first attempt with raw matrix
    {

      m |>
        solve(tol = tolerance()) |>
        diag()

    },

    #on error, use edited matrix
    error = function(e){

      #look for perfect correlations that break solve()
      #and replace them with 0.99 or -0.99
      m.range <- range(
        m[upper.tri(m)]
      )

      #maximum and minimum correlation
      max.cor <- 0.9999999999
      min.cor <- -max.cor

      #replace values
      if(max(m.range) > max.cor){
        m[m > max.cor] <- max.cor
        diag(m) <- 1
      }

      if(min(m.range) < min.cor){
        m[m < min.cor] <- min.cor
      }

      tryCatch(
        {
          m |>
            solve(tol = tolerance()) |>
            diag()
        },
        error = function(e) {
          return(NULL)
        }
      )

    }

  )

  if(is.null(out)){

    warning(
      "\n",
      function_name,
      ": the correlation matrix is singular and cannot be solved. Returning Inf VIFs",
      call. = FALSE
    )

    out <- rep(x = Inf, times = ncol(m))
    names(out) <- colnames(m)

    return(out)

  }

  out <- sort(
    x = abs(out),
    decreasing = TRUE
    ) |>
    round(digits = 4)

  out

}
