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
#' #subset to limit example run time
#' df <- vi[1:1000, ]
#' predictors <- vi_predictors[1:10]
#' predictors_numeric <- vi_predictors_numeric[1:10]
#'
#' #predictors has mixed types
#' sapply(
#'   X = df[, predictors, drop = FALSE],
#'   FUN = class
#' )
#'
#' #categorical predictors are ignored
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors,
#'   max_vif = 2.5
#' )
#'
#' x
#'
#' #all these have a VIF lower than max_vif (2.5)
#' vif_df(
#'   df = df,
#'   predictors = x
#' )
#'
#'
#' #higher max_vif results in larger selection
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors_numeric,
#'   max_vif = 10
#' )
#'
#' x
#'
#'
#' #smaller max_vif results in smaller selection
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors_numeric,
#'   max_vif = 2.5
#' )
#'
#' x
#'
#'
#' #custom preference order
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors_numeric,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_temperature_mean",
#'     "topo_elevation"
#'   ),
#'   max_vif = 2.5
#' )
#'
#' x
#'
#' #using automated preference order
#' df_preference <- preference_order(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors_numeric
#' )
#'
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors_numeric,
#'   preference_order = df_preference,
#'   max_vif = 2.5
#' )
#'
#' x
#'
#'
#' #categorical predictors are ignored
#' #the function returns NA
#' x <- vif_select(
#'   df = df,
#'   predictors = vi_predictors_categorical
#' )
#'
#' x
#'
#'
#' #if predictors has length 1
#' #selection is skipped
#' #and data frame with one row is returned
#' x <- vif_select(
#'   df = df,
#'   predictors = predictors_numeric[1]
#' )
#'
#' x
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

  if(!is.logical(quiet)){
    message("\ncollinear::vif_select(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  #do nothing if
  #  one predictor only
  #  max_vif is NULL
  if(is.null(max_vif)){

    if(quiet == FALSE){

      message("\ncollinear::vif_select(): argument 'max_vif' is NULL, skipping VIF-based filtering.")

    }

    return(predictors)
  }

  #checking argument max_vif
  if(
    !is.numeric(max_vif) ||
    length(max_vif) != 1 ||
    max_vif < 2.5 ||
    max_vif > 10
  ){

    if(quiet == FALSE){

      message("\ncollinear::vif_select(): invalid 'max_vif', resetting it to 5.")

    }

    max_vif <- 5
  }

  #validate data
  predictors <- validate_data_vif(
    df = df,
    predictors = predictors,
    function_name = "collinear::vif_select()",
    quiet = quiet
  )

  #if no numerics, return predictors
  if(length(predictors) <= 1){

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
        "\ncollinear::vif_select(): maximum VIF in 'predictors' is <= ",
        max_vif,
        ". skipping VIF-based filtering."
        )

    }

    return(predictors)

  }

  #because vif_df returns higher VIF first
  preference_order_auto <- rev(preference_order_auto$predictor)

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto,
    function_name = "collinear::vif_select()",
    quiet = quiet
  )

  #fast function to compute max vif
  if(capabilities("long.double") == TRUE){
    tolerance = 0
  } else {
    tolerance = .Machine$double.eps
  }

  fast_max_vif <- function(
    df = NULL,
    predictors = NULL,
    tolerance = NULL
  ){

    stats::cor(
      x = df[, predictors, drop = FALSE],
      use = "complete.obs",
      method = "pearson"
    ) |>
      solve(tol = tolerance) |>
      diag() |>
      max()

  }

  #vectors with selected and candidates
  selected <- preference_order[1]
  candidates <- preference_order[-1]

  #iterate over candidate variables
  for(candidate in candidates){

    #use fast option first
    #use slow option if error
    vif.max <- tryCatch(
      {
        #fast option as default
        fast_max_vif(
          df = df,
          predictors = c(
            selected,
            candidate
          ),
          tolerance = tolerance
        )
      },
      error = function(e) {
        #slower option on error
        vif.df <- vif_df(
          df = df,
          predictors = c(
            selected,
            candidate
          )
        )
        return(max(vif.df$vif))
      }
    )


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
      "\ncollinear::vif_select(): selected predictors: \n - ",
      paste(selected, collapse = "\n - ")
    )

  }

  attr(
    x = selected,
    which = "validated"
  ) <- TRUE

  selected

}
