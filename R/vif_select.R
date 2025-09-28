#' @title Automated Multicollinearity Filtering with Variance Inflation Factors
#'
#' @description
#'
#' This function automatizes multicollinearity filtering in data frames with numeric predictors by combining two methods:
#' \itemize{
#'
#'   \item **Preference Order**: System to rank predictors and protect important ones during multicollinearity filtering. The function offers two alternative options:
#'
#'   \itemize{
#'
#'     \item Argument \code{preference_order}: Helps preserve predictors of interest for the user. Requires a character vector of predictors names. When two predictors in this vector are highly correlated, the one with a higher index is removed.
#'
#'
#'    \item If \code{preference_order} is NULL, predictors are ranked from lower to higher VIF, as returned by [vif_df()]. This option preserves rare predictors over redundant ones, but does not guarantee strong statistical models.
#'
#'   }
#'
#'    \item **VIF-based Filtering**: Computes Variance Inflation Factors for numeric predictors and removes redundant ones iteratively, while taking preference order into account. See [vif()] and [vif_df()] for further details.
#' }
#'
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
#'     responses = "vi_numeric",
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
#' @family variance_inflation_factor
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
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif_select()",
    ... = ...
  )

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

  #validate predictors for vif analysis
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

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": at least two predictors are required for VIF filtering."
      )

    }

    return(predictors)
  }

  #validate preference order
  preference.order <- validate_arg_preference_order(
    df = df,
    predictors = predictors,
    preference_order = preference_order,
    function_name = function_name,
    quiet = quiet
  )

  preference.order <- preference.order$predictor

  #vectors with selected and candidates
  selected <- preference.order[1]
  candidates <- preference.order[-1]

  #iterate over candidate variables
  for(candidate in candidates){

    #compute correlation matrix
    vif.max <- cor_matrix(
      df = df,
      predictors = c(selected, candidate),
      quiet = quiet,
      function_name = function_name
    ) |>
      vif(function_name = function_name) |>
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
