#' @title Automated Multicollinearity Filtering with Pairwise Correlations
#'
#' @description
#'
#' Please check the section **Pairwise Correlation Filtering** at the end of this help file for further details.
#' @inheritSection collinear Pairwise Correlation Filtering
#'
#' @inheritParams collinear
#' @inherit collinear return
#' @examples
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
#'
#' #mixed predictors
#' vi_predictors <- vi_predictors[1:10]
#'
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableWorkers() - 1
#' )
#'
#' #progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #without preference order
#' selected_predictors <- cor_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   max_cor = 0.75
#' )
#'
#' selected_predictors
#'
#' #with manual preference order
#' selected_predictors <- cor_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_type"
#'   ),
#'   max_cor = 0.75
#' )
#'
#' selected_predictors
#'
#' #soil_type dissappears because its correlation
#' #with swi_mean is above max_cor
#' cor_df(
#'   df = vi,
#'   predictors = c(
#'     "swi_mean",
#'     "soil_type"
#'   )
#' )
#'
#'
#' #with automated preference order
#' #using a response variable
#' df_preference <- preference_order(
#'   df = vi,
#'   response = "vi_numeric",
#'   predictors = vi_predictors
#' )
#'
#' selected_predictors <- cor_select(
#'   df = vi,
#'   predictors = vi_predictors,
#'   preference_order = df_preference,
#'   max_cor = 0.75
#' )
#'
#' selected_predictors
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_select <- function(
    df = NULL,
    predictors = NULL,
    preference_order = NULL,
    cor_method = "pearson",
    max_cor = 0.75
){

  #do nothing if one predictor only
  if(length(predictors) == 1){
    return(predictors)
  }

  #checking argument max_cor
  if(max_cor < 0 || max_cor > 1){
    stop("Argument 'max_cor' must be a numeric between 0 and 1.")
  }

  #correlation matrix
  m <- cor_matrix(
    df = df,
    predictors = predictors,
    cor_method = cor_method
  ) |>
    abs()

  #auto preference order
  #variables with lower sum of correlation with others go higher
  preference_order_auto <- m |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  #return reversed to facilitate next operation
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  ) |>
    rev()

  #organize the correlation matrix according to preference_order
  m <- m[
    preference_order,
    preference_order
    ]

  #set diag to 0
  diag(m) <- 0

  #vector of selected variables
  selected <- preference_order

  for(candidate in preference_order){

    #if candidate vs selected is over the cor threshold
    if(max(m[selected, candidate]) > max_cor){

      #remove candidate
      selected <- setdiff(
        x = selected,
        y = candidate
      )

    }

  }

  #reverse to order as preference order
  selected <- rev(selected)

  attr(
    x = selected,
    which = "validated"
  ) <- TRUE

  selected

}
