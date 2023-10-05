#' Pairwise correlation
#'
#' @description Returns the pairwise Pearson correlation between all pairs of predictors in a training dataset.
#'
#' Please note that near-zero variance columns are ignored by this function. Use [mc_auto_vif()] to remove them.
#'
#' @param df (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param predictors (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `df`. Default: `NULL`
#' @param method (optional; charater string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#'
#' @return Data frame with pairs of predictors and their correlation.
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions,
#'   ecoregions_predictors
#' )
#'
#' df <- cor_df(
#'       df = ecoregions,
#'       predictors = ecoregions_predictors
#' )
#'
#' }
#' @autoglobal
#' @export
cor_df <- function(
    df = NULL,
    predictors = NULL,
    method = NULL
){

  #check predictors
  predictors <- predictors_inspect(
    df = df,
    predictors = predictors
  )

  #check input data frame
  df <- df_inspect(
    df = df[, predictors],
    min_rows = ifelse(
      test = method == "pearson",
      yes = 30,
      no = 10
    ))

  #correlation matrix
  #TODO: function cor_matrix using target encoding for categoricals
  cor.matrix <- stats::cor(
    x = df,
    use = "pairwise.complete.obs",
    method = method
  ) |>
    abs() |>
    round(3)

  #selecting upper triangle only
  cor.matrix.upper.tri <- upper.tri(cor.matrix)

  #not upper tri to na
  cor.matrix[!cor.matrix.upper.tri] <- NA

  #to long format
  cor.df <- cor.matrix |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "a") |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(predictors),
      names_to = "b",
      values_to = "cor"
    ) |>
    stats::na.omit() |>
    dplyr::arrange(
      dplyr::desc(cor)
    )

  if(return.df == TRUE){
    cor.df <- as.data.frame(cor.df)
  }

  #returning output
  cor.df

}
