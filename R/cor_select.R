#' @title Automated multicollinearity reduction via pairwise correlation
#'
#' @description
#'
#' Applies a recursive forward selection algorithm algorithm to select predictors with a bivariate correlation with any other predictor lower than a threshold defined by the argument `max_cor`.
#'
#' If the argument `response` is provided, all non-numeric variables in `predictors` are transformed into numeric using target encoding (see [target_encoding_lab()]). Otherwise, non-numeric variables are ignored.
#'
#' The argument `preference_order` allows defining a preference selection order to preserve (when possible) variables that might be interesting or even required for a given analysis. If NULL, predictors are ordered from lower to higher sum of their absolute pairwise correlation with the other predictors.
#'
#' For example, if `predictors` is `c("a", "b", "c")` and `preference_order` is `c("a", "b")`, there are two possibilities:
#' \itemize{
#'  \item If the correlation between `"a"` and `"b"` is below `max_cor`, both variables are selected.
#'  \item If their correlation is equal or above `max_cor`, then `"a"` is selected, no matter its correlation with `"c"`,
#' }
#'
#' If `preference_order` is not provided, then the predictors are ranked by their variance inflation factor as computed by `vif_df()`.
#'
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param preference_order  (optional; character vector) vector with column names in 'predictors' in the desired preference order, or result of the function [preference_order()]. Allows defining a priority order for selecting predictors, which can be particularly useful when some predictors are more critical for the analysis than others. Default: NULL (predictors ordered from lower to higher sum of absolute correlation with the other predictors).
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param max_cor (optional; numeric) Maximum correlation allowed between any pair of predictors. Higher values return larger number of predictors with higher multicollinearity. Default: 0.75
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#' @return Character vector with the names of the selected predictors.
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#' vi_predictors <- vi_predictors[1:10]
#'
#' #without response
#' #without preference_order
#' #permissive max_cor
#' selected.predictors <- cor_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_cor = 0.8
#' )
#'
#' selected.predictors
#'
#' #without response
#' #without preference_order
#' #restrictive max_cor
#' selected.predictors <- cor_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_cor = 0.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #without preference_order
#' #restrictive max_cor
#' #slightly different solution than previous one
#' #because here target encoding is done against the response
#' #while before was done pairwise against each numeric predictor
#' selected.predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   max_cor = 0.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #with user-defined preference_order
#' #restrictive max_cor
#' #numerics and categorical variables in output
#' selected.predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   preference_order = c(
#'     "soil_type", #categorical variable
#'     "soil_temperature_mean",
#'     "swi_mean",
#'     "rainfall_mean",
#'     "evapotranspiration_mean"
#'   ),
#'   max_cor = 0.5
#' )
#'
#' selected.predictors
#'
#'
#' #with response
#' #with automated preference_order
#' #restrictive max_cor and max_vif
#' #numerics and categorical variables in output
#' preference.order <- preference_order(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   f = f_rsquared #cor(response, predictor)
#' )
#'
#' head(preference.order)
#'
#' selected.predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   preference_order = preference.order,
#'   max_cor = 0.5
#' )
#'
#' selected.predictors
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
cor_select <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    cor_method = "pearson",
    max_cor = 0.75,
    encoding_method = "mean"
){

  #checking argument max_cor
  if(max_cor < 0 || max_cor > 1){
    stop("argument 'max_cor' must be a numeric between 0 and 1.")
  }

  #correlation data frame
  cor.df <- cor_df(
    df = df,
    response = response,
    predictors = predictors,
    cor_method = cor_method,
    encoding_method = encoding_method
  )

  #correlation matrix
  cor.matrix <- cor_matrix(
    df = cor.df
  ) |>
    abs()

  #auto preference order
  #variables with lower sum of cor with others go higher
  preference_order.auto <- cor.matrix |>
    colSums() |>
    sort() |>
    names()

  #if there is no preference order
  if(is.null(preference_order)){
    preference_order <- preference_order.auto
  }

  #check if preference_order comes from preference_order()
  if(is.data.frame(preference_order) == TRUE){
    if("predictor" %in% names(preference_order)){
      preference_order <- preference_order$predictor
    } else {
      stop("argument 'preference_order' must be a data frame with the column 'predictor'.")
    }
  }

  #subset preference_order in predictors
  if(!is.null(predictors)){
    preference_order <- preference_order[preference_order %in% predictors]
  }

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

  #i for first iteration
  i <- 1

  #iterate over i values
  while(i < ncol(cor.matrix)){

    i <- i + 1

    #remove i column and row if max > max_cor
    if(max(cor.matrix[1:i, i]) > max_cor){

      #remove column
      cor.matrix <- cor.matrix[-i, -i, drop = FALSE]

      #break condition
      if(ncol(cor.matrix) == 1){break}

      #go back one column
      i <- i - 1

    }

  }

  #return names of selected variables
  colnames(cor.matrix)

}
