#' @title Automated Multicollinearity Filtering with Variance Inflation Factors
#'
#' @description
#'
#' This function automatizes multicollinearity filtering in data frames with numeric predictors by combining two methods:
#' \itemize{
#' \item **Preference Order**: method to rank and preserve relevant variables during  multicollinearity filtering. See argument \code{preference_order} and function [preference_order()].
#' \item **VIF-based filtering**: recursive algorithm to identify and remove predictors with a VIF above a given threshold.
#' }
#'
#' When the argument \code{preference_order} is not provided, the predictors are ranked lower to higher VIF. The predictor selection resulting from this option, albeit diverse and uncorrelated, might not be the one with the highest overall predictive power when used in a model.
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
#'   data(vi, vi_predictors)
#'
#'   df <- vi[1:1000, ]
#'
#'   #predictors has mixed types
#'   sapply(
#'     X = df[, vi_predictors, drop = FALSE],
#'     FUN = class
#'   ) |>
#'     unique()
#'
#'   #categorical predictors are ignored
#'   selected_predictors <- vif_select(
#'     df = df,
#'     predictors = vi_predictors,
#'     max_vif = 5,
#'     quiet = FALSE
#'   )
#'
#'   #all these have a VIF lower than max_vif (2.5)
#'   vif_df(
#'     df = df,
#'     predictors = selected_predictors,
#'     quiet = TRUE
#'   )
#'
#'   #custom preference order
#'   selected_predictors <- vif_select(
#'     df = df,
#'     predictors = vi_predictors,
#'     preference_order = c(
#'       "swi_mean",
#'       "soil_temperature_mean",
#'       "topo_elevation",
#'       "wrong_name" #ignored
#'     ),
#'     max_vif = 2.5,
#'     quiet = FALSE
#'   )
#'
#'   #using automated preference order
#'   df_preference <- preference_order(
#'     df = df,
#'     response = "vi_numeric",
#'     predictors = vi_predictors[1:10]
#'   )
#'
#'   selected_predictors <- vif_select(
#'     df = df,
#'     predictors = vi_predictors,
#'     preference_order = df_preference,
#'     max_vif = 5,
#'     quiet = FALSE
#'   )
#' @autoglobal
#' @family vif
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
    max_vif = 5,
    quiet = FALSE
){

  function_name <- "collinear::vif_select()"

  df <- validate_arg_df(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  if(is.null(max_vif)){
    return(NULL)
  }

  predictors <- validate_arg_predictors_vif(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  if(
    length(predictors) == 1 ||
    is.null(predictors)
  ){
    return(predictors)
  }

  #auto preference order
  preference_order_auto <- vif_df(
    df = df,
    predictors = predictors,
    quiet = quiet
  )

  if(max(preference_order_auto$vif) <= max_vif){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": maximum VIF is <= ",
        max_vif,
        ", skipping VIF filtering."
      )

    }

    attr(predictors, "validated_vif") <- NULL

    return(predictors)

  }

  #reorder because vif_df returns higher VIF first
  preference_order_auto <- rev(preference_order_auto$predictor)

  #validate preference order
  preference_order <- validate_arg_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto,
    function_name = function_name,
    quiet = quiet
  )

  #vectors with selected and candidates
  selected <- preference_order[1]
  candidates <- preference_order[-1]

  #iterate over candidate variables
  for(candidate in candidates){

    #compute correlation matrix
    vif.max <- cor_matrix(
      df = df,
      predictors = c(selected, candidate),
      quiet = quiet
    ) |>
      vif() |>
      max()


    #if candidate keeps vif below the threshold
    if(vif.max <= max_vif){

      #add candidate to selected
      selected <- c(
        selected,
        candidate
      )

    }

  }

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": selected predictors: \n - ",
      paste(selected, collapse = "\n - ")
    )

  }

  attr(
    x = selected,
    which = "validated"
  ) <- TRUE

  selected

}
