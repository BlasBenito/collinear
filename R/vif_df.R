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
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#'
#' @return Data frame with predictor names and VIF values
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions,
#'   ecoregions_predictors
#' )
#'
#' df <- vif_df(
#'       df = ecoregions,
#'       predictors = ecoregions_predictors
#' )
#'
#' }
#' @autoglobal
#' @export
vif_df <- function(
    df = NULL,
    response = NULL,
    predictors = NULL
){

  #for development only
  # df <- ecoregions
  # response <- "plant_richness"
  # predictors <- ecoregions_predictors

  #check input data frame
  df <- df_inspect(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- predictors_inspect(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- response_inspect(
    df = df,
    response = response
  )

  #factors, logical, and ordered to characters
  df <- rapply(
    object = df[, c(response, predictors)],
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  #target encode character predictors
  df <- target_encode(
    df = df,
    response = response,
    predictors = predictors
  )

  #get numeric predictors only
  predictors.numeric <- predictors_numeric(
    df = df,
    predictors = predictors
  )

  #vif data frame
  tryCatch(
    {
      vif.df <- data.frame(
        diag(
          solve(
            cor(
              x = df[, predictors.numeric],
              use = "complete.obs"
            ),
            tol = 0
          )
        ),
        stringsAsFactors = FALSE
      ) |>
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





