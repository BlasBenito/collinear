#' Correlation matrix of numeric and character variables
#'
#' @description Returns a correlation matrix between all pairs of predictors in a training dataset. Non-numeric predictors are transformed into numeric via target encoding, using the 'response' variable as reference.
#'
#' @details
#' This function attempts to handle correlations between pairs of variables that can be of different types:
#' \itemize{
#'   \item numeric vs. numeric: computed with stats::cor() with the methods "pearson" or "spearman".
#'   \item numeric vs. character, two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variable is target-encoded as numeric using the values of the response as reference, and then its correlation with the numeric variable is computed with stats::cor(). This option generates a response-specific result suitable for training statistical and machine-learning models
#'     \item 'response' is NULL (or the name of a non-numeric column): the character variable is target-encoded as numeric using the values of the numeric predictor (instead of the response) as reference, and then their correlation is computed with stats::cor(). This option leads to a response-agnostic result suitable for clustering problems.
#'   }
#'   \item character vs. character, two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variables are target-encoded as numeric using the values of the response as reference, and then their correlation is computed with stats::cor().
#'     \item response' is NULL (or the name of a non-numeric column): the association between the character variables is computed using Cramer's V. This option might be problematic, because R-squared values and Cramer's V, even when having the same range between 0 and 1, are not fully comparable.
#'   }
#' }
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#'
#' @return correlation matrix
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
#' vi_predictors <- vi_predictors[1:5]
#'
#' #convert correlation data frame to matrix
#' df <- cor_df(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' m <- cor_matrix(
#'   df = df
#' )
#'
#' #show first three columns and rows
#' m[1:5, 1:5]
#'
#' #generate correlation matrix directly
#' m <- cor_matrix(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' m[1:5, 1:5]
#'
#' #with response (much faster)
#' #different solution than previous one
#' #because target encoding is done against the response
#' #rather than against the other numeric in the pair
#' m <- cor_matrix(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors
#' )
#'
#' m[1:5, 1:5]
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
cor_matrix <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    cor_method = "pearson",
    encoding_method = "mean"
){

  #if df with predictors, compute cor data frame
  if(all(names(df) %in% c("x", "y", "correlation")) == FALSE){

    df <- cor_df(
      df = df,
      response = response,
      predictors = predictors,
      cor_method = cor_method,
      encoding_method = encoding_method
    )

  }

  #create all possible pairs
  df <- df |>
    dplyr::mutate(
      x = df$y,
      y = df$x
    ) |>
    rbind(df)

  #rows and col names
  variables <- unique(c(df$x, df$y))

  #empty square matrix
  m <- matrix(
    data = NA,
    nrow = length(variables),
    ncol = length(variables)
    )

  #named vector to map row/column names to indices
  index_map <- stats::setNames(
    object = seq_along(variables),
    nm = variables
    )

  #vectorized indexing to fill in the matrix
  m[
    cbind(
      index_map[df$x],
      index_map[df$y]
      )
    ] <- df$correlation

  #dim names
  rownames(m) <- variables
  colnames(m) <- variables

  #replace NA in diag with 1
  diag(m) <- 1

  m

}
