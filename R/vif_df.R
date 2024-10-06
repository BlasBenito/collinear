#' @title Variance Inflation Factor
#'
#' @description
#'
#' Computes the Variance Inflation Factor of numeric variables in a data frame.
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
#' @return data frame; predictors names their VIFs
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
#' @inherit vif_select references
#' @export
vif_df <- function(
    df = NULL,
    predictors = NULL
){

  #internal function to compute VIF
  #from correlation matrix
  f_vif <- function(m = NULL){

    if(capabilities("long.double") == TRUE){
      tolerance = 0
    } else {
      tolerance = .Machine$double.eps
    }

    #compute VIF
    df <- m |>
      solve(tol = tolerance) |>
      diag() |>
      data.frame(stringsAsFactors = FALSE)

    #format data frame
    colnames(df) <- "vif"
    df$vif <- round(abs(df$vif), 4)
    df$predictor <- colnames(m)
    rownames(df) <- NULL

    #arrange by VIF
    df[
      order(df$vif),
      c("predictor", "vif")
    ]

  }

  #check input data frame
  df <- validate_df(
    df = df
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  #get numeric predictors only
  predictors <- identify_predictors_numeric(
    df = df,
    predictors = predictors
  )

  #validate data dimensions
  predictors <- validate_data_vif(
    df = df,
    predictors = predictors,
    function_name = "collinear::vif_df()"
  )

  #if no numerics, return predictors
  if(length(predictors) == 0){
    message("collinear::vif_df(): no numeric predictors available, returning NA.")
    return(NA)
  }

  if(length(predictors) == 1){
    return(
      data.frame(
        variable = predictors,
        vif = 0
      )
    )
  }

  #compute correlation matrix
  m <- stats::cor(
    x = df[, predictors, drop = FALSE],
    use = "complete.obs"
  )

  #first try
  vif.df <- tryCatch(
    {f_vif(m = m)},
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
        f_vif(m = m)

        },
      error = function(e) {

        stop(
          "collinear::vif_df(): The correlation matrix is singular and cannot be solved. Removing highly correlated predictors with collinear::cor_select() beforehand may help fix this issue.",
          call. = FALSE
          )

      }
    )

  }

  vif.df

}





