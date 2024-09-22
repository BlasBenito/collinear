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
#'   vi_predictors
#' )
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #reduce correlation in predictors with cor_select()
#' vi_predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   max_cor = 0.75
#' )
#'
#' #without response
#' #only numeric predictors are returned
#' df <- vif_df(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' df
#'
#' #with response
#' #categorical and numeric predictors are returned
#' df <- vif_df(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors
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
    response = NULL,
    predictors = NULL,
    encoding_method = "mean"
){

  #function to compute vif
  vif_f <- function(m = NULL){

    if(capabilities("long.double") == TRUE){
      tolerance = 0
    } else {
      tolerance = .Machine$double.eps
    }

    df <- m |>
      solve(tol = tolerance) |>
      diag() |>
      data.frame(stringsAsFactors = FALSE) |>
      dplyr::rename(vif = 1) |>
      dplyr::transmute(
        variable = colnames(m),
        vif = round(abs(vif), 3)
      ) |>
      dplyr::arrange(vif)

    rownames(df) <- NULL

    df

  }

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
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

  #target encode character predictors
  df <- target_encoding_lab(
    df = df,
    response = response,
    predictors = predictors,
    encoding_methods = encoding_method,
    replace = TRUE,
    verbose = FALSE
  )

  #get numeric predictors only
  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  #compute correlation matrix
  cor.matrix <- stats::cor(
    x = df[, predictors.numeric, drop = FALSE],
    use = "complete.obs"
  )

  #first try
  vif.df <- tryCatch(
    {vif_f(m = cor.matrix)},
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
        cor.matrix.range <- range(
          cor.matrix[upper.tri(cor.matrix)]
          )

        #maximum and minimum correlation
        max.cor <- 0.9999999999
        min.cor <- -max.cor

        #replace values
        if(max(cor.matrix.range) > max.cor){
          cor.matrix[cor.matrix > max.cor] <- max.cor
          diag(cor.matrix) <- 1
        }

        if(min(cor.matrix.range) < min.cor){
          cor.matrix[cor.matrix < min.cor] <- min.cor
        }

        #compute vif with the new matrix
        vif_f(m = cor.matrix)

        },
      error = function(e) {

        stop("The correlation matrix is singular and cannot be solved. This issue may be fixed by removing highly correlated variables with collinear::cor_select() before the VIF analysis.")

      }
    )

  }

  vif.df

}





