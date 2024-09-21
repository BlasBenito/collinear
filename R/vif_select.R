#' @title Automated Multicollinearity Management via Variance Inflation Factors
#'
#' @description
#'
#' Automates multicollinearity management by selecting variables based on their Variance Inflation Factor (VIF).
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all other predictors in the input data frame as predictors. The VIF equation can be interpreted as the "rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' If the `response` argument is provided, and there are categorical variables named in the `predictors` argument, then these variables are transformed to numeric via Target Encoding (see [target_encoding_lab()]). If `response` is not provided, then categorical variables are ignored.
#'
#'
#' The function [vif_select()] applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max_vif`).
#'
#' If the argument `response` is provided, all non-numeric variables in `predictors` are transformed into numeric using target encoding (see [target_encoding_lab()]). Otherwise, non-numeric variables are ignored.
#'
#' The argument `preference_order` defines a ranking of preference to preserve (when possible) variables that might be interesting or even required for a given analysis.
#'
#' For example, if `predictors` is `c("a", "b", "c")` and `preference_order` is `c("a", "b")`, there are two possibilities:
#' \itemize{
#'  \item If the VIF of `"a"` is higher than the VIF of `"b"`, and both VIF values are above `max_vif`, then `"a"` is selected and `"b"` is removed.
#'  \item If their correlation is equal or above `max_cor`, then `"a"` is selected, no matter its correlation with `"c"`,
#' }
#'
#' If `preference_order` is not provided, then the predictors are ranked by their variance inflation factor as computed by [vif_df()].
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
#' #reduce correlation in predictors with cor_select()
#' vi_predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   max_cor = 0.75
#' )
#'
#' #without response
#' #without preference_order
#' #permissive max_vif
#' #only numeric predictors are processed
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_vif = 10
#' )
#'
#' selected.predictors
#'
#' #without response
#' #without preference_order
#' #restrictive max_vif
#' #only numeric predictors are processed
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #without preference_order
#' #restrictive max_cor
#' #slightly different solution than previous one
#' #because categorical variables are target-enccoded
#' selected.predictors <- vif_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' #with response
#' #with user-defined preference_order
#' #restrictive max_cor
#' #numerics and categorical variables in output
#' selected.predictors <- vif_select(
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
#'   max_vif = 2.5
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
#' selected.predictors <- vif_select(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   preference_order = preference.order,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' @autoglobal
#' @family vif
#' @author Blas M. Benito, PhD
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. \doi{10.1002/0471725153}.
#' }
#' @export
vif_select <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_vif = 5,
    encoding_method = "mean"
){

  #checking argument max_vif
  if(max_vif < 2.5 || max_vif > 10){
    if(max_vif < 0){max_vif <- 0}
    message("Recommended values for 'max_vif' are between 2.5 and 10.")
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
  if(length(predictors)  == 1){
    attributes(predictors) <- NULL
    return(predictors)
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

  #auto preference order
  #variables with lower sum of cor with others go higher
  preference_order_auto <- vif_df(
    df = df,
    response = response,
    predictors = predictors
  )$variable

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  )

  #order df according to preference order
  df <- df[, preference_order, drop = FALSE]

  #rank of interest
  df.rank <- data.frame(
    variable = colnames(df),
    rank = seq_len(ncol(df))
  )

  for(i in seq(from = nrow(df.rank), to = 2)){

    vif.i <- vif_df(
      df = df,
      predictors = df.rank$variable
    ) |>
      dplyr::filter(
        variable == df.rank[i, "variable"]
      ) |>
      dplyr::pull(vif)

    #removing var if vif is above max_vif
    if(vif.i > max_vif){

      #removing it from df.rank
      df.rank <- dplyr::filter(
        df.rank,
        variable != df.rank[i, "variable"]
      )

    }

  }

  #selected variables
  df.rank$variable

}
