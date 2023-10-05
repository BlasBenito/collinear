#' Correlation matrix
#'
#' @description Returns a correlation matrix between all pairs of predictors in a training dataset. Non-numeric predictors are transformed into numeric via target encoding.
#'
#'
#' @param df (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
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
cor_matrix <- function(
    df = NULL,
    method = NULL
){

  #method argument for stats::cor
  method <- match.arg(
    arg = method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  #check input data frame
  df <- df_inspect(
    df = df,
    min_rows = ifelse(
      test = method == "pearson",
      yes = 30,
      no = 10
    ))

  #identify numerics and non-numerics
  predictors.numeric <- predictors_numeric(df = df)
  predictors.non.numeric <- predictors_non_numeric(df = df)

  #correlation matrix for numeric variables
  cor.matrix.numeric <- stats::cor(
    x = df[, predictors.numeric],
    use = "pairwise.complete.obs",
    method = method
  )

  dimnames(x = cor.matrix.numeric) <- predictors.numeric

  #return output if there are only numeric variables
  if(length(predictors.non.numeric) == 0){
    return(cor.matrix.numeric)
  }

  #correlation matrix between numerics and non.numerics
  cor.df <- expand.grid(
    numeric = predictors.numeric,
    non.numeric = predictors.non.numeric,
    correlation = NA,
    stringsAsFactors = FALSE
  )

  for(i in seq_len(nrow(cor.df))){

    df.i <- fe_target_encoding(
      df = df,
      response = cor.df$numeric[i],
      predictors = cor.df$non.numeric[i],
      methods = "mean",
      replace = TRUE,
      verbose = FALSE
    )

  }


}
