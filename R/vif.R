#' Variance Inflation Factors From Correlation Matrix
#'
#' @description Computes the Variance Inflation Factors from a correlation matrix in two steps:
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

      #compute minimum eigenvalue
      min_eigen <- min(eigen(m, only.values = TRUE)$values)

      #apply ridge regularization if needed
      if(min_eigen < 0.001){

        #shift eigenvalues to ensure positive definiteness
        ridge <- abs(min_eigen) + 0.001
        m_adj <- m + diag(ridge, nrow(m))

      } else {
        m_adj <- m
      }

      #try again with adjusted matrix
      tryCatch(
        {diag(solve(m_adj, tol = tol))},
        error = function(e2) {
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
  vif_cap <- 1e6
  if(any(out > vif_cap, na.rm = TRUE)){
    if(quiet == FALSE){
      message(
        "\n",
        function_name,
        ": some VIF values exceeded 1M and were set to Inf."
      )
    }
    out[out > vif_cap] <- Inf
  }

  #add names
  names(out) <- colnames(m)

  #sort
  out <- sort(
    x = out,
    decreasing = TRUE
  ) |>
    round(digits = 4)


  out

}
