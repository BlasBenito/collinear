#' Correlation matrix of numeric and character variables
#'
#' @description Returns a correlation matrix frame between all pairs of predictors in a training dataset, or transforms the result of `cor_df()` to matrix. Please read the documentation of `cor_df()` to better understand how the correlations between numeric and character predictors are computed.
#'
#' @param df (required; data frame or tibble) AA data frame with numeric and/or character predictors predictors, and optionally, a response variable, or the result of `cor_df()`. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Ignored if 'df' is the result of `cor_df()`. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If omitted, all columns of 'df' are used as predictors. Ignored if 'df' is the result of `cor_df()`. Default:'NULL'
#' @param method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Ignored if 'df' is the result of `cor_df()`. Default: "pearson".
#'
#' @return correlation matrix
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions,
#'   ecoregions_predictors
#' )
#'
#' m <- cor_matrix(
#'       df = ecoregions,
#'       predictors = ecoregions_predictors
#'   )
#'
#' }
#' @autoglobal
#' @export
cor_matrix <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    method = "pearson"
){

  #if df with predictors, compute cor data frame
  if(all(names(df) %in% c("x", "y", "correlation")) == FALSE){

    df <- cor_df(
      df = df,
      response = response,
      predictors = predictors,
      method = method
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
    object = 1:length(variables),
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
