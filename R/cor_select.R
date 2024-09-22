#' @title Automated multicollinearity reduction via pairwise correlation
#'
#' @description
#'
#' Automates multicollinearity management in data frames with numeric and non-numeric predictors by combining three methods:
#' \itemize{
#' \item **Target Encoding**: transforms non-numeric predictors to numeric using another numeric variable (usually a model response) as reference. See [target_encoding_lab()].
#' \item **Preference Order**: method to rank and preserve relevant variables during  multicollinearity filtering. See [preference_order()].
#' \item **Pairwise Correlation Filtering**: automated logic to identify pairs of highly correlated variables.
#' }
#'
#' This function calls these other functions:
#' \itemize{
#'   \item [target_encoding_lab()]: to apply target encoding, if `response` is provided and there are categorical variables named in `predictors`.
#'   \item [cor_df()]: to compute bivariate correlations between all pairs of predictors.
#' }
#'
#' Please check the sections **Target Encoding**, **Preference Order**, and **Pairwise Correlation Filtering** at the end of this help file for further details.
#'
#' @inheritSection collinear Target Encoding
#' @inheritSection collinear Preference Order
#' @inheritSection collinear Pairwise Correlation Filtering
#'
#' @inheritParams collinear
#' @inherit collinear return
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
#' #restrictive max_cor
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
#' @family correlation
#' @author Blas M. Benito, PhD
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

  #do nothing if one predictor only
  if(length(predictors) == 1){
    attributes(predictors) <- NULL
    return(predictors)
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
  preference_order_auto <- cor.matrix |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  )

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
