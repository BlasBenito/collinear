#' @title Automated Multicollinearity Management
#' @description
#' Automated version of [collinear()], with the complete functionality but fewer arguments for fire-and-forget usage.
#'
#' Arguments \code{max_vif} and \code{max_cor} are autoconfigured depending on the average correlation between all pairs of predictors. The autoconfiguration rules are shown below:
#' \preformatted{
#'
#' max_cor <- max(
#'   0.5,
#'   correlation_mean
#' )
#'
#' if (max_cor >= 0.95) {
#'   max_vif <- 10
#' } else if (max_cor >= 0.85) {
#'   max_vif <- 7.5
#' } else if (max_cor >= 0.75) {
#'   max_vif <- 5
#' } else {
#'   max_vif <- 2.5
#' }
#' }

#' @inheritParams collinear
#' @examples
#'
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#' )
#'
#' x <- collinear_auto(
#'   df = vi_smol,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors_numeric
#' )
#'
#' x
#'
#' @inherit collinear return
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @export
collinear_auto <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    quiet = FALSE
){

  function_name <- "collinear::collinear_auto()"

  #autoconfiguration
  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  predictors <- validate_arg_predictors(
    df = df,
    response = NULL,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  correlation_stats <- cor_stats(
    df = df,
    predictors = predictors,
    quiet = TRUE
  )

  correlation_mean <- correlation_stats$stats[
    correlation_stats$stats$statistic == "mean",
    "value"
  ]

  max_cor <- max(
    0.5,
    correlation_mean
  )

  if (max_cor >= 0.95) {
    max_vif <- 10
  } else if (max_cor >= 0.85) {
    max_vif <- 7.5
  } else if (max_cor >= 0.75) {
    max_vif <- 5
  } else {
    max_vif <- 2.5
  }

  max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  if(quiet == FALSE){

    message(
      "Autoconfiguration finished:\n",
      "- max_vif = ", max_vif,
      "\n- max_cor = ", max_cor
    )

  }

  #call to collinear
  out <- collinear(
    df = df,
    responses = responses,
    predictors = predictors,
    encoding_method = "loo",
    preference_order = NULL,
    f = f_auto,
    max_cor = max_cor,
    max_vif = max_vif,
    quiet = quiet,
    function_name = function_name
  )

  out

}
