#' Variance Inflation Factors From Absolute Correlation Matrix
#'
#' @description Computes the Variance Inflation Factors from an absolute correlation matrix in two steps:
#' \itemize{
#'   \item Applies [base::solve()] to transform the absolute correlation matrix into a precision matrix, which is the inverse of the covariance matrix between all variables in \code{predictors}.
#'   \item Applies [base::diag()] to extract the diagonal of the precision matrix, which contains the variance of the regression of each predictor against all other predictors, also known as Variance Inflation Factor
#'}
#'
#' @inheritSection collinear Variance Inflation Factors
#'
#' @param m (required, matrix) Correlation matrix generated via [stats::cor()] or [cor_matrix()]. Must have named dimensions. Default: NULL
#' @inheritParams collinear
#'
#' @return named numeric vector
#' @family variance_inflation_factor
#' @inherit vif_select references
#' @autoglobal
#' @export
#' @examples
#' data(vi_smol, vi_predictors_numeric)
#'
#' m <- cor_matrix(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric[1:5]
#' )
#'
#' vif(m)
vif <- function(
    m = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif()",
    ... = ...
  )

  if(!"matrix" %in% class(m)){

    stop(
      "\n",
      function_name,
      ": argument 'm' must be a correlation matrix generated with 'stats::cor()' or 'collinear::cor_matrix()'.",
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

  #condition number
  kappa_m <- kappa(m, exact = TRUE)
  if(kappa_m > 1e10 && quiet == FALSE){
    message(
      "\n",
      function_name,
      ": VIF values may be numerically unstable due to severe multicollinearity."
    )
  }

  #tol arg of solve()
  #prevents both false singularity detection and numerical instability
  tol <- min(
    .Machine$double.eps * nrow(m) * max(abs(m)),
    sqrt(.Machine$double.eps)
    )

  out <- tryCatch(
    #first attempt with original matrix
    {diag(solve(m, tol = tol))},
    #try with adjusted matrix
    error = function(e){
      #look for perfect correlations that break solve()
      # and replace them with slightly smaller values
      m_adj <- m
      m_range <- range(m[upper.tri(m)])

      # Threshold for "too perfect" correlation
      max_cor <- 0.9999999999
      min_cor <- -max_cor

      #replace extreme values
      if(max(m_range) > max_cor){
        m_adj[m_adj > max_cor] <- max_cor
        diag(m_adj) <- 1
      }
      if(min(m_range) < min_cor){
        m_adj[m_adj < min_cor] <- min_cor
      }

      # try again with adjusted matrix
      tryCatch(
        {diag(solve(m_adj, tol = tol))},
        error = function(e) {
          warning(
            "\n",
            function_name,
            ": the correlation matrix is singular and cannot be solved, returning infinite VIF scores.",
            call. = FALSE
          )

          out <- rep(Inf, times = ncol(m))

          names(out) <- colnames(m)

          return(out)
        }
      )
    }
  )

  # cap
  out[out > .Machine$integer.max ] <- Inf

  #add names
  names(out) <- colnames(m)

  #sort
  out <- sort(
    x = abs(out),
    decreasing = TRUE
  ) |>
    round(digits = 4)


  out

}
