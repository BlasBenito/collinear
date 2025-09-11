#' @title Automated Multicollinearity Management
#'
#' @description
#' A fire-and-forget wrapper to [collinear()]. It configures target encoding, preference order, and the maximum multicollinearity levels (\code{max_vif} and \code{max_cor}) automatically.
#'
#' Target encoding is triggered with the method "loo" (leave-one-out, see [target_encoding_lab()] for further details) when a response is numeric and there are categorical predictors.
#'
#' Preference order is computed automatically when \code{responses} is not NULL using [f_auto()] to select a proper method depending on the nature of the response and the predictors.
#'
#' Arguments \code{max_vif} and \code{max_cor} are autoconfigured depending on the average correlation between all pairs of predictors. The autoconfiguration rules are shown below:
#'
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
#'   max_vif <- 8.5
#' } else if (max_cor >= 0.80) {
#'   max_vif <- 7
#' } else if (max_cor >= 0.75) {
#'   max_vif <- 5
#' } else if (max_cor >= 0.70) {
#'   max_vif <- 3.5
#' } else {
#'   max_vif <- 2.5
#' }
#'
#' }
#'
#' These rules were obtained empirically, by comparing executions of [vif_select()] and [cor_select()] on 100000 different combinations of rows and columns of the dataset [vi], and finding the \code{max_vif} value leading to the most similar variable selection resulting from a given \code{max_cor} value.
#'
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

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": autoconfiguring 'max_vif' and 'max_cor'."
    )

  }

  correlation_stats <- cor_stats(
    df = df,
    predictors = predictors,
    quiet = TRUE
  )

  correlation_mean <- correlation_stats$stats[
    correlation_stats$stats$statistic == "mean",
    "value"
  ]

  max_vif <- NULL
  max_cor <- max(
    0.5,
    round(x = correlation_mean, digits = 2)
  )

  max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  if(max_cor > 0.90 && quiet == FALSE){

    message(
      "\n",
      function_name,
      ": the average pairwise correlation in this dataset (",
      round(x = max_cor, digits = 2),
      ") is very high, automated multicollinearity filtering might yield suboptimal results in this case."
    )

  }

  #configure max vif
  predictors.numeric <- identify_predictors_numeric(
    df = df,
    predictors = predictors,
    quiet = quiet
  )

  if(
    !is.null(predictors.numeric) &&
    length(predictors.numeric) > 1
  ){

    if (max_cor >= 0.95) {
      max_vif <- 10
    } else if (max_cor >= 0.85) {
      max_vif <- 8.5
    } else if (max_cor >= 0.80) {
      max_vif <- 7
    } else if (max_cor >= 0.75) {
      max_vif <- 5
    } else if (max_cor >= 0.70) {
      max_vif <- 3.5
    } else {
      max_vif <- 2.5
    }

    max_vif <- validate_arg_max_vif(
      max_vif = max_vif,
      function_name = function_name,
      quiet = quiet
    )

  }

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": autoconfiguration finished:\n",
      "- max_vif = ", ifelse(
        test = is.null(max_vif),
        yes = "NULL",
        no = max_vif
      ),
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
