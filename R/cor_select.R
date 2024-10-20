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
#' predictors <- vi_predictors[1:10]
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
#'   cor_max = 0.75
#' )
#'
#' x
#'
#' #with custom preference order
#' x <- cor_select(
#'   df = df,
#'   predictors = predictors,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_type"
#'   ),
#'   cor_max = 0.75
#' )
#'
#' x
#'
#' #soil_type dissappears because its correlation
#' #with swi_mean is above cor_max
#' cor_df(
#'   df = df,
#'   predictors = c(
#'     "swi_mean",
#'     "soil_type"
#'   )
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
#'   cor_max = 0.75
#' )
#'
#' x
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
    cor_method = "pearson",
    cor_max = 0.75,
    quiet = FALSE
){

  if(!is.logical(quiet)){
    message("\ncollinear::cor_select(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  #do nothing if one predictor only
  if(is.null(cor_max)){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): argument 'cor_max' is NULL, skipping pairwise correlation filtering.")

    }

    return(predictors)

  }

  #checking argument cor_max
  if(
    !is.numeric(cor_max) ||
    length(cor_max) != 1 ||
    cor_max < 0.1 ||
    cor_max > 1){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): invalid 'cor_max', resetting it to 0.75.")

    }

    cor_max <- 0.75
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
    predictors = predictors,
    cor_method = cor_method
  ) |>
    abs()

  #test to skip computation if needed
  if(max(m[upper.tri(x = m)]) <= cor_max){

    if(quiet == FALSE){

      message("\ncollinear::cor_select(): maximum pairwise correlation is <= ", cor_max, ", skipping pairwise correlation filtering.")

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
  #return reversed to facilitate next operation
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto,
    function_name = "collinear::cor_select()",
    quiet = quiet
  ) |>
    rev()


  if(quiet == FALSE){

    message("\ncollinear::cor_select(): running pairwise correlation filtering.")

  }

  #organize the correlation matrix according to preference_order
  m <- m[
    preference_order,
    preference_order
  ]

  #set diag to 0
  diag(m) <- 0

  #vector of selected variables
  selected <- preference_order

  #iterate over candidate variables
  for(candidate in preference_order){

    #if candidate vs selected is over the cor threshold
    if(max(m[selected, candidate]) > cor_max){

      #remove candidate
      selected <- setdiff(
        x = selected,
        y = candidate
      )

    }

  }

  #reverse to order as preference order
  selected <- rev(selected)

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
