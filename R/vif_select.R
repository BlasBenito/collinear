#' @title Automated multicollinearity reduction via Variance Inflation Factor
#'
#' @description
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all other predictors in the input data frame as predictors. The VIF equation can be interpreted as the "rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The function `vif_select()` applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max_vif`).
#'
#' If the argument `response` is provided, all non-numeric variables in `predictors` are transformed into numeric using target encoding (see `target_encode()`). Otherwise, non-numeric variables are ignored.
#'
#' The argument `preference_order` allows defining a preference selection order to preserve (when possible) variables that might be interesting or even required for a given analysis.
#'
#' For example, if `predictors` is `"c("a", "b", "c")` and `preference_order` is `"c("a", "b")`, there are two possibilities:
#' \itemize{
#'  \item If the VIF of `"a"` is higher than the VIF of `"b"`, and both VIF values are above `max_vif`, then `"a"` is selected and `"b"` is removed.
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
#' @param max_vif (optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: `5`.

#' @return Character vector with the names of uncorrelated predictors.
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
#'  selected.variables <- vif_select(
#'    data = vi,
#'    predictors = vi_predictors
#'  )
#'
#'  selected.variables
#'
#'  #with preference order
#'  selected.variables <- vif_select(
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
vif_select <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_vif = 5
){

  #dev args
  # df <- vi
  # response <- "vi_mean"
  # predictors <- vi_predictors
  # preference_order <- vi_predictors[1:5]
  # max_vif <- 5

  #checking argument max_vif
  if(max_vif < 2.5 | max_vif > 10){
    if(max_vif < 0){max_vif <- 0}
    warning("Recommended values for the 'max_vif' argument are between 2.5 and 10.")
  }

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

  #target encode character predictors
  df <- target_encode(
    df = df,
    response = response,
    predictors = predictors
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

  #order df according to preference order
  df <- df[, preference_order]

  #rank of interest
  df.rank <- data.frame(
    variable = colnames(df),
    rank = 1:ncol(df)
  )

  #iterating through reversed preference order
  for(i in seq(from = nrow(df.rank), to = 2)){

    vif.i <- vif_df(
      df = df[, df.rank$variable]
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




