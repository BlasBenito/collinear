#' @title Automated multicollinearity reduction via pairwise correlation
#'
#' @description Applies a recursive algorithm to remove variables with a bivariate correlation with any other variable higher than a threshold defined by the argument `max_cor`.
#'
#' If the argument `response` is provided, all non-numeric variables in `predictors` are transformed into numeric using target encoding (see `target_encode()`). Otherwise, non-numeric variables are ignored.
#'
#' The argument `preference_order` allows defining a preference selection order to preserve (when possible) variables that might be interesting or even required for a given analysis
#'
#' For example, if `predictors` is `"c("a", "b", "c")` and `preference_order` is `"c("a", "b")`, there are two possibilities:
#' \itemize{
#'  \item If the correlation between `"a"` and `"b"` is below `max_cor`, both variables are selected.
#'  \item If their correlation is equal or above `max_cor`, then `"a"` is selected, no matter its correlation with `"c"`,
#' }
#'
#' If `preference_order` is not provided, then the predictors are ranked by their variance inflation factor as computed by `vif_df()`.
#'
#'
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param preference_order  (optional; character vector) vector with column names of 'df' in the desired preference order. Predictors not included in this argument are ranked by their Variance Inflation Factor. Default: `NULL`.
#' @param method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation between any pair of the selected variables. Higher values return larger number of predictors with higher multicollinearity. Default: `0.75`
#' @return Character vector with the names of the selected predictors.
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(
#'   vi,
#'   vi_predictors
#'   )
#'
#'  #without preference order
#'  selected.variables <- cor_select(
#'    data = vi,
#'    predictors = vi_predictors
#'  )
#'
#'  selected.variables
#'
#'  #with preference order
#'  selected.variables <- cor_select(
#'    data = vi,
#'    predictors = vi_predictors,
#'    preference_order = vi_predictors[1:5],
#'  )
#'
#'  selected.variables
#'
#' }
#' @autoglobal
#' @export
cor_select <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    method = "pearson",
    max_cor = 0.75
){

  #checking argument max_cor
  if(max_cor < 0 | max_cor > 1){
    stop("Argument 'max_cor' must be a numeric between 0 and 1.")
  }

  #correlation data frame
  cor.df <- cor_df(
    df = df,
    response = response,
    predictors = predictors,
    method = method
  )

  #correlation matrix
  cor.matrix <- cor_matrix(
    df = cor.df
  ) |>
    abs()

  #auto preference order
  #variables with lower sum of cor with others go higher
  preference_order.auto <- vif_df(
    df = df,
    response = response,
    predictors = predictors
  )$variable

  #if there is no preference order
  if(is.null(preference_order)){

    preference_order <- preference_order.auto

  }

  #check preference order
  preference_order <- predictors_inspect(
    df = df,
    predictors = preference_order
  )

  #if there are variables not in preference_order
  #add them in the order of preference_order.auto
  if(length(preference_order) < length(predictors)){

    not.in.preference_order <- setdiff(
      x = predictors,
      y = preference_order
    )

    preference_order <- c(
      preference_order,
      preference_order.auto[
        preference_order.auto %in% not.in.preference_order
      ]
    )

  }

  #organize the correlation matrix according to preference_order
  cor.matrix <- cor.matrix[
    preference_order,
    preference_order
    ]

  #set diag to 0
  diag(cor.matrix) <- 0

  #iterating through columns
  for(i in seq(from = ncol(cor.matrix), to = 1)){

    #remove i column and row if max > max_cor
    if(max(cor.matrix[, i]) > max_cor){
      cor.matrix <- cor.matrix[-i, -i]
    }

  }

  #return names of selected variables
  colnames(cor.matrix)

}
