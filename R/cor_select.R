#' @title Automated Multicollinearity Filtering with Absolute Pairwise Correlations
#'
#' @description
#'
#' Reduces multicollinearity among predictors using the following steps:
#'
#' - Computes the pairwise correlations between predictors.
#' - Orders predictors according to the user preference (argument \code{preference_order}) or from lower to higher multicollinearity otherwise.
#' - Iteratively selects predictors:
#'   - Starts with the top-preference predictor.
#'   - Adds predictors one by one only if their correlation with already selected predictors does not exceed `max_cor`.
#' - Returns the set of selected predictors.
#'
#' Please check the section **Pairwise Correlation Filtering** at the end of this help file for further details.
#'
#' @inheritSection collinear Pairwise Correlation Filtering
#'
#' @inheritParams collinear
#' @inherit collinear return
#' @examples
#' data(vi_smol)
#'
#' #predictors
#' predictors = c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
#'
#' #OPTIONAL: parallelization setup
#' # only worth it for large data
#' # future::plan(
#' #   future::multisession,
#' #   workers = 2
#' # )
#' #
#' #OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #predictors ordered from lower to higher multicollinearity
#' x <- cor_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   max_cor = 0.7
#' )
#'
#' x
#'
#'
#' #with custom preference order
#' x <- cor_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = c(
#'     "koppen_zone",
#'     "soil_type"
#'   ),
#'   max_cor = 0.7
#' )
#'
#'
#' #with automated preference order
#' df_preference <- preference_order(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = predictors
#' )
#'
#' df_preference
#'
#' x <- cor_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = df_preference,
#'   max_cor = 0.7
#' )
#'
#' #OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_select <- function(
    df = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_cor = 0.7,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_select()",
    ... = ...
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  if(is.null(max_cor)){
    return(NULL)
  }

  df <- validate_arg_df(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  predictors <- validate_arg_predictors_cor(
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

  m <- cor_matrix(
    df = df,
    predictors = predictors,
    function_name = function_name
  ) |>
    abs()

  #test to skip computation if needed
  if(max(m[upper.tri(x = m)]) <= max_cor){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": maximum pairwise correlation is <= ", max_cor, ", skipping correlation filtering."
        )

    }

    attr(predictors, "validated_cor") <- NULL

    return(predictors)

  }

  #auto preference order
  #variables with lower sum of correlation with others go higher
  preference_order_auto <- m |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  preference.order <- validate_arg_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto,
    function_name = function_name,
    quiet = quiet
  )

  if(
    is.data.frame(preference.order) &&
    "predictor" %in% colnames(preference.order)
  ){
    preference.order <- preference.order$predictor
  }

  #organize the correlation matrix according to preference_order
  m <- m[
    preference.order,
    preference.order
  ]

  #set diag to 0
  diag(m) <- 0

  #vectors with selected and candidates
  selected <- preference.order[1]
  candidates <- preference.order[-1]

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
