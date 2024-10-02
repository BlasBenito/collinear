#' @title Variance Inflation Factor
#'
#' @description
#'
#' Computes the Variance Inflation Factor of all variables in a training data frame.
#'
#' This function computes the VIF (see section **Variance Inflation Factors** below) in two steps:
#' \itemize{
#'   \item Applies [base::solve()] to obtain the precision matrix, which is the inverse of the covariance matrix between all variables in `predictors`.
#'   \item Uses [base::diag()] to extract the diagonal of the precision matrix, which contains the variance of the prediction of each predictor from all other predictors, and represents the VIF.
#' }
#'
#' @inheritSection collinear Variance Inflation Factors
#'
#' @inheritParams collinear
#' @return data frame; predictors names and VIF scores
#'
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors_numeric
#' )
#'
#' df <- vif_df(
#'   df = vi[1:1000, ], #subset to limit run time
#'   predictors = vi_predictors_numeric
#' )
#'
#' df
#'
#' @autoglobal
#' @family vif
#' @author Blas M. Benito, PhD
#' @inherit vif_select references
#' @export
vif_df <- function(
    df = NULL,
    predictors = NULL
){

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  #early output if only one predictor
  if(length(predictors) == 1){
    return(
      data.frame(
        variable = predictors,
        vif = 0
      )
    )
  }

  #get numeric predictors only
  predictors.numeric <- identify_predictors_numeric(
    df = df,
    predictors = predictors
  )

  #compute correlation matrix
  m <- stats::cor(
    x = df[, predictors.numeric, drop = FALSE],
    use = "complete.obs"
  )

  #first try
  vif.df <- tryCatch(
    {vif(m = m)},
    error = function(e) {
      return(NA)
    }
  )

  #second try
  if(is.data.frame(vif.df) == FALSE){

    vif.df <- tryCatch(
      {

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

        #compute vif with the new matrix
        vif(m = m)

        },
      error = function(e) {

        stop(
          "collinear::vif_df(): The correlation matrix is singular and cannot be solved. This issue may be fixed by removing highly correlated variables with collinear::cor_select() before the VIF analysis.",
          call. = FALSE
          )

      }
    )

  }

  vif.df

}





