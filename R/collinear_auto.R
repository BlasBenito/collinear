#' @title Automated Multicollinearity Management
#'
#' @description
#' A fire-and-forget wrapper to [collinear()]. It configures target encoding, preference order, and the maximum multicollinearity levels (\code{max_vif} and \code{max_cor}) automatically.
#'
#' Target encoding is triggered with the method "loo" (leave-one-out, see [target_encoding_lab()] for further details) when a response is numeric and there are categorical predictors.
#'
#' Preference order is computed automatically when \code{responses} is not NULL using [f_auto()] to select a proper method depending on the nature of the response and the predictors.
#'
#' Arguments \code{max_vif} and \code{max_cor} are autoconfigured by the functions [autoconfig_arg_max_cor()] and [autoconfig_arg_max_vif()] depending on the overall pairwise correlation between predictors.
#'
#' @inheritParams collinear
#' @param max_cor  (optional, numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. If NULL, [autoconfig_arg_max_cor()] finds a suitable value. Default: NULL
#' @param max_vif (optional, numeric or NULL). Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. If NULL, [autoconfig_arg_max_vif()] finds a suitable value for the given data. Default: NULL
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
    max_cor = NULL,
    max_vif = NULL,
    quiet = FALSE
){

  function_name <- "collinear::collinear_auto()"

  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet
  )

  predictors <- validate_arg_predictors(
    df = df,
    response = NULL,
    predictors = predictors,
    quiet = quiet
  )

  #autoconfiguration
  max_cor <- autoconfig_arg_max_cor(
    df = df,
    predictors = predictors,
    max_cor = max_cor,
    quiet = quiet
  )

  max_vif <- autoconfig_arg_max_vif(
    df = df,
    predictors = predictors,
    max_cor = max_cor,
    max_vif = max_vif,
    quiet = quiet
  )

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
    quiet = quiet
  )

  out

}
