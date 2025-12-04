#' @title Automated Multicollinearity Filtering with Pairwise Correlations
#'
#' @description
#'
#' Wraps [collinear_select()] to automatize multicollinearity filtering via pairwise correlation in dataframes with numeric and categorical predictors.
#'
#' The argument \code{max_cor} determines the maximum variance inflation factor allowed in the resulting selection of predictors.
#'
#' The argument \code{preference_order} accepts a character vector of predictor names ranked from first to last index, or a dataframe resulting from [preference_order()]. When two predictors in this vector or dataframe are highly collinear, the one with a lower ranking is removed. This option helps protect predictors of interest. If not provided, predictors are ranked from lower to higher multicollinearity.
#'
#' Please check the section **Pairwise Correlation Filtering** at the end of this help file for further details.
#'
#' @inheritSection collinear Pairwise Correlation Filtering
#'
#' @inheritParams collinear_select
#' @return character vector of selected predictors
#' @examples
#' data(vi_smol)
#'
#' ## OPTIONAL: parallelization setup
#' ## irrelevant when all predictors are numeric
#' ## only worth it for large data with many categoricals
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' ## OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #predictors
#' predictors = c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
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
#' x
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
#' x
#'
#' #OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family multicollinearity_filtering
#' @author Blas M. Benito, PhD
#' @export
cor_select <- function(
  df = NULL,
  response = NULL,
  predictors = NULL,
  preference_order = NULL,
  max_cor = 0.7,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_select()",
    function_name = dots$function_name
  )

  if (is.null(max_cor)) {
    stop(
      "\n",
      function_name,
      ": argument 'max_cor' cannot be NULL.",
      call. = FALSE
    )
  }

  out <- collinear_select(
    df = df,
    response = response,
    predictors = predictors,
    preference_order = preference_order,
    max_cor = max_cor,
    max_vif = NULL,
    quiet = quiet,
    function_name = function_name,
    m = dots$m
  )

  out
}
