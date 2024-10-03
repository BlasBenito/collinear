#' @title Automated Multicollinearity Filtering with Variance Inflation Factors
#'
#' @description
#'
#' This function automatizes multicollinearity filtering in data frames with numeric predictors by combining two methods:
#' \itemize{
#' \item **Preference Order**: method to rank and preserve relevant variables during  multicollinearity filtering. See argument `preference_order` and function [preference_order()].
#' \item **VIF-based filtering**: recursive algorithm to identify and remove predictors with a VIF above a given threshold.
#' }
#'
#' When the argument `preference_order` is not provided, the predictors are ranked lower to higher VIF. The predictor selection resulting from this option, albeit diverse and uncorrelated, might not be the one with the highest overall predictive power when used in a model.
#'
#' Please check the sections **Preference Order**, **Variance Inflation Factors**, and **VIF-based Filtering** at the end of this help file for further details.
#'
#'
#' @inheritSection collinear Preference Order
#' @inheritSection collinear Variance Inflation Factors
#' @inheritSection collinear VIF-based Filtering
#'
#' @inheritParams collinear
#' @inherit collinear return
#' @examples
#' data(vi)
#'
#' #reduce data size
#' vi <- vi[1:1000, ]
#'
#' #if argument predictors is not provided
#' #all numeric variables in df are used
#' selected.predictors <- vif_select(
#'   df = vi
#' )
#'
#' selected.predictors
#'
#'
#' #otherwise, only variables in predictors are selected
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = c(
#'     "swi_mean",
#'     "rainfall_mean",
#'     "evapotranspiration_mean",
#'     "humidity_mean"
#'   ),
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#' #all these have a VIF lower than max_vif (2.5)
#' vif_df(
#'   df = vi,
#'   predictors = selected.predictors
#' )
#'
#'
#' #higher max_vif results in larger selection
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_numeric,
#'   max_vif = 10
#' )
#'
#' selected.predictors
#'
#'
#' #smaller max_vif results in smaller selection
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_numeric,
#'   max_vif = 2.5
#' )
#'
#' selected.predictors
#'
#'
#' #using manual preference order
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_numeric,
#'   preference_order = c(
#'     "swi_mean",
#'     "rainfall_mean",
#'     "evapotranspiration_mean",
#'     "humidity_mean"
#'   ),
#'   max_vif = 2.5
#' )
#'
#' #only "humidity_mean" is lost from the results
#' selected.predictors
#'
#'
#' #using automated preference order
#' df_preference <- preference_order(
#'   df = vi,
#'   response = "vi_numeric",
#'   predictors = vi_predictors_numeric
#' )
#'
#' df_preference
#'
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_numeric,
#'   preference_order = df_preference,
#'   max_vif = 10
#' )
#'
#' selected.predictors
#'
#'
#' #categorical predictors are ignored
#' #the function returns NA
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_categorical
#' )
#'
#' selected.predictors
#'
#'
#' #if predictors has length 1
#' #selection is skipped
#' #and data frame with one row is returned
#' selected.predictors <- vif_select(
#'   df = vi,
#'   predictors = vi_predictors_numeric[1]
#' )
#'
#' selected.predictors
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @author Blas M. Benito, PhD
#' @references
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. DOI: 10.1002/0471725153.
#' }
#' @export
vif_select <- function(
    df = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_vif = 5
){

  #do nothing if
  #  one predictor only
  #  max_vif is NULL
  if(is.null(max_vif)){
    message("collinear::vif_select(): multicollinearity filter disabled (max_vif = NULL), returning all predictors.")
    return(predictors)
  }

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

  #check predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  #identify numerics
  predictors.numeric <- identify_predictors_numeric(
    df = df,
    predictors = predictors
  )

  #if no numerics, return predictors
  if(length(predictors.numeric) == 0){
    message("collinear::vif_select(): no numeric predictors available, returning NA.")
    return(NA)
  }

  if(length(predictors.numeric) == 1){
    message("collinear::vif_select(): only one numeric predictor available, skipping multicollinearity filtering.")
    return(
      data.frame(
        variable = predictors,
        vif = 0
      )
    )
  }

  #auto preference order
  #variables with lower sum of cor with others go higher
  preference_order_auto <- vif_df(
    df = df,
    predictors = predictors
  )$predictor

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  )

  #subset df to preference order
  df <- df[, preference_order, drop = FALSE]

  #vector of selected variables
  preference_order_selected <- preference_order[1]

  #vector of candidate variables
  preference_order_candidates <- preference_order[-1]

  #TODO: choose forward or backward option
  #forward recursive VIF filtering
  while(length(preference_order_candidates) > 0){

    #generate VIF data frame
    vif.df <- vif_df(
      df = df,
      predictors = c(
        preference_order_selected,
        preference_order_candidates[1]
      )
    )

    #add candidate to selected
    if(max(vif.df[["vif"]]) <= max_vif){
      preference_order_selected <- c(
        preference_order_selected,
        preference_order_candidates[1]
      )
    }

    #remove candidate
    preference_order_candidates <- preference_order_candidates[-1]

  }

  # #rank of interest
  # df.rank <- data.frame(
  #   predictor = preference_order,
  #   rank = seq_len(ncol(df))
  # )
  #
  # #backwards recursive VIF filtering
  # for(i in seq(from = nrow(df.rank), to = 2)){
  #
  #   #generate VIF data frame
  #   vif.i.df <- vif_df(
  #     df = df,
  #     predictors = df.rank[["predictor"]]
  #   )
  #
  #   #extract relevant vif value
  #   vif.i <- vif.i.df[["vif"]][
  #     vif.i.df[["predictor"]] == df.rank[["predictor"]][i]
  #     ]
  #
  #   #removing rank row if vif higher than max_vif
  #   if(vif.i > max_vif){
  #     df.rank <- df.rank[-i, ]
  #   }
  #
  # }
  #
  # #selected variables
  # out <- df.rank[["predictor"]]

  attr(
    x = preference_order_selected,
    which = "validated"
  ) <- TRUE

  preference_order_selected

}
