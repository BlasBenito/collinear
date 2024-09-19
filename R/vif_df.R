#' @title Variance Inflation Factor
#'
#' @description
#'
#' Computes the Variance Inflation Factor of all variables in a training data frame.
#'
#' Warning: predictors with perfect correlation might cause errors, please use [cor_select()] to remove perfect correlations first.
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity.
#'
#' This function computes the Variance Inflation Factor (VIF) in two steps:
#' \itemize{
#'   \item Applies `\link[base]{solve}` to obtain the precision matrix, which is the inverse of the covariance matrix.
#'   \item Uses `\link[base]{diag}` to extract the diagonal of the precision matrix, which contains the variance of the prediction of each predictor from all other predictors.
#' }
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#' @return Data frame with predictor names and VIF values
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
#' @author Blas M. Benito
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. \doi{10.1002/0471725153}.
#' }
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





