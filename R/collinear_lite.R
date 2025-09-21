#' @title Minimal Wrapper for \code{collinear}
#' @description
#' Minimalistic version of [collinear()] with the following features:
#' \itemize{
#'   \item Argument \code{responses} not required.
#'   \item Target encoding disabled.
#'   \item Computation of preference order disabled. Only the argument \code{preference_order} is available.
#'   \item Output is a character vector of selected predictors
#' }
#' @inheritParams collinear
#' @inheritParams target_encoding_lab
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#' )
#'
#' x <- collinear_lite(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric,
#'   preference_order = c(
#'     "aridity_index",
#'     "soil_temperature_mean"
#'     ),
#'   max_cor = 0.5,
#'   max_vif = 2.5
#' )
#'
#' x
#'
#' @return character vector: predictors selection
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @export
collinear_lite <- function(
    df = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_cor = 0.70,
    max_vif = 5,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear_lite()",
    ... = ...
  )

  #call to collinear
  out <- collinear(
    df = df,
    responses = NULL,
    predictors = predictors,
    encoding_method = NULL,
    preference_order = preference_order,
    f = NULL,
    max_cor = max_cor,
    max_vif = max_vif,
    quiet = quiet,
    function_name = function_name
  )

  out <- out$result$selection

  out

}
