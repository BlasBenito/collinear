#' @title Automated multicollinearity reduction via Variance Inflation Factor
#'
#' @description
#'
#' Automates multicollinearity management by selecting variables based on their Variance Inflation Factor (VIF).
#'
#' The [vif_select()] function is designed to automate the reduction of multicollinearity in a set of predictors by using Variance Inflation Factors.
#'
#' If the 'response' argument is provided, categorical predictors are converted to numeric via target encoding (see [target_encoding_lab()]). If the 'response' argument is not provided, categorical variables are ignored.
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all other predictors in the input data frame as predictors. The VIF equation can be interpreted as the "rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The function [vif_select()] applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max_vif`).
#'
#' If the argument `response` is provided, all non-numeric variables in `predictors` are transformed into numeric using target encoding (see [target_encoding_lab()]). Otherwise, non-numeric variables are ignored.
#'
#' The argument `preference_order` allows defining a preference selection order to preserve (when possible) variables that might be interesting or even required for a given analysis.
#'
#' For example, if `predictors` is `c("a", "b", "c")` and `preference_order` is `c("a", "b")`, there are two possibilities:
#' \itemize{
#'  \item If the VIF of `"a"` is higher than the VIF of `"b"`, and both VIF values are above `max_vif`, then `"a"` is selected and `"b"` is removed.
#'  \item If their correlation is equal or above `max_cor`, then `"a"` is selected, no matter its correlation with `"c"`,
#' }
#'
#' If `preference_order` is not provided, then the predictors are ranked by their variance inflation factor as computed by [vif_df()].
#'
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param preference_order  (optional; character vector) vector with column names in 'predictors' in the desired preference order, or result of the function [preference_order()]. Allows defining a priority order for selecting predictors, which can be particularly useful when some predictors are more critical for the analysis than others. Predictors not included in this argument are ranked by their Variance Inflation Factor. Default: NULL.
#' @param max_vif (optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed for any given predictor in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.
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
#' @author Blas M. Benito
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
    warning("the recommended values for the argument 'max_vif' are between 2.5 and 10.")
  }

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
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

  #check if preference_order comes from preference_order()
  if(is.data.frame(preference_order) == TRUE){
    preference_order <- preference_order$predictor
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

  #order df according to preference order
  df <- df[, preference_order, drop = FALSE]

  #rank of interest
  df.rank <- data.frame(
    variable = colnames(df),
    rank = seq_len(ncol(df))
  )

  #iterating through reversed preference order
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
