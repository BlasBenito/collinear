#' @title Automated Multicollinearity Filtering with Pairwise Correlations
#'
#' @description
#'
#' Implements a recursive forward selection algorithm to keep predictors with a maximum pairwise correlation with all other selected predictors lower than a given threshold. Uses [cor_df()] underneath, and as such, can handle different combinations of predictor types.
#'
#' Please check the section **Pairwise Correlation Filtering** at the end of this help file for further details.
#'
#' @inheritSection collinear Pairwise Correlation Filtering
#'
#' @inheritParams collinear
#' @inherit collinear return
#' @examples
#' #subset to limit example run time
#' df <- vi[1:1000, ]
#'
#' #only numeric predictors only to speed-up examples
#' #categorical predictors are supported, but result in a slower analysis
#' predictors <- vi_predictors_numeric[1:8]
#'
#' #predictors has mixed types
#' sapply(
#'   X = df[, predictors, drop = FALSE],
#'   FUN = class
#' )
#'
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #without preference order
#' x <- cor_select(
#'   df = df,
#'   predictors = predictors,
#'   max_cor = 0.75
#' )
#'
#'
#' #with custom preference order
#' x <- cor_select(
#'   df = df,
#'   predictors = predictors,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_type"
#'   ),
#'   max_cor = 0.75
#' )
#'
#'
#' #with automated preference order
#' df_preference <- preference_order(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors
#' )
#'
#' x <- cor_select(
#'   df = df,
#'   predictors = predictors,
#'   preference_order = df_preference,
#'   max_cor = 0.75
#' )
#'
#' #resetting to sequential processing
#' future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_select <- function(
    df = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_cor = 0.75,
    quiet = FALSE
){

  if(!is.logical(quiet)){
    message("\ncollinear::cor_select(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  #do nothing if one predictor only
  if(is.null(max_cor)){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): argument 'max_cor' is NULL, skipping pairwise correlation filtering.")

    }

    return(predictors)

  }

  #checking argument max_cor
  if(
    !is.numeric(max_cor) ||
    length(max_cor) != 1 ||
    max_cor < 0.1 ||
    max_cor > 1
    ){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): invalid 'max_cor', resetting it to 0.75.")

    }

    max_cor <- 0.75
  }

  #validate input data
  predictors <- validate_data_cor(
    df = df,
    predictors = predictors,
    function_name = "collinear::cor_select()",
    quiet = quiet
  )

  if(length(predictors) <= 1){
    return(predictors)
  }

  #correlation matrix
  if(quiet == FALSE){

    message("\ncollinear::cor_select(): computing pairwise correlation matrix.")

  }

  m <- cor_matrix(
    df = df,
    predictors = predictors
  ) |>
    abs()

  #test to skip computation if needed
  if(max(m[upper.tri(x = m)]) <= max_cor){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): maximum pairwise correlation is <= ", max_cor, ", skipping pairwise correlation filtering.")

    }

    return(predictors)

  }


  #auto preference order
  #variables with lower sum of correlation with others go higher
  preference_order_auto <- m |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto,
    function_name = "collinear::cor_select()",
    quiet = quiet
  )

  #organize the correlation matrix according to preference_order
  m <- m[
    preference_order,
    preference_order
  ]

  #set diag to 0
  diag(m) <- 0

  #vectors with selected and candidates
  selected <- preference_order[1]
  candidates <- preference_order[-1]

  #iterate over candidate variables
  for(candidate in candidates){

    #if candidate keeps correlation below the threshold
    if(max(m[selected, candidate]) <= max_cor){

      #add candidate to selected
      selected <- c(
        selected,
        candidate
        )

    }

  }

  if(quiet == FALSE){

    message(
      "\ncollinear::cor_select(): selected predictors: \n - ",
      paste(selected, collapse = "\n - ")
    )

  }

  attr(
    x = selected,
    which = "validated"
  ) <- TRUE

  selected

}
