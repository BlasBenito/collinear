#' @title Variance Inflation Factor
#'
#' @description
#'
#' Computes the Variance Inflation Factor of all variables in a training data frame.
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity.
#'
#' This function computes the Variance Inflation Factor (VIF) in three steps:
#' \itemize{
#'   \item Computes the correlation matrix between all pairs of predictors using `stats::cor()`.
#'   \item Applies `solve()` to obtain the precision matrix, which is the inverse of the covariance matrix.
#'   \item Uses `diag()` to extract the diagonal of the precision matrix, which contains the variance of the prediction of each predictor from all other predictors.
#' }
#'
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: `"mean"`
#' @return Data frame with predictor names and VIF values
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
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
#' }
#' @autoglobal
#' @export
vif_df <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    encoding_method = "mean"
){

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

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

  #vif data frame
  tryCatch(
    {

      vif.df <- df[, predictors.numeric, drop = FALSE] |>
        stats::cor(use = "complete.obs") |>
        solve(tol = 0) |>
        diag() |>
        data.frame(stringsAsFactors = FALSE) |>
        dplyr::rename(vif = 1) |>
        dplyr::transmute(
          variable = predictors.numeric,
          vif = round(abs(vif), 3)
        ) |>
        dplyr::arrange(vif)

      rownames(vif.df) <- NULL

    }, error = function(e) {
      stop("the VIF computation failed. Please check for perfect correlations between predictors, or an excessive number of NA values in the 'df' argument.")
    }
  )

  vif.df

}




