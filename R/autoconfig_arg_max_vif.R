#' Autoconfigure Argument \code{max_vif}
#'
#' If \code{max_vif = NULL}, autoconfigures it depending on the value of \code{max_cor}, if provided, or the average pairwise correlation of the numeric predictors as follows:
#'
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
#' These rules were obtained empirically, by comparing executions of [vif_select()] and [cor_select()] on 100000 different combinations of rows and columns of the dataset [vi] and \code{max_cor} values, and finding the \code{max_vif} value leading to the most similar variable selection resulting from a given \code{max_cor}.
#'
#' @inheritParams collinear
#' @param max_vif optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2 (strict) and 10 (permissive). If NULL, the value is autoconfigured depending on the average pairwise correlation of the dataset.
#' @inheritParams validate_arg_quiet
#'
#' @returns numeric
#' @export
#' @autoglobal
#' @examples
#' max_vif <- autoconfig_arg_max_vif(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric,
#'   max_vif = NULL
#' )
#'
#' max_vif
autoconfig_arg_max_vif <- function(
    df = NULL,
    predictors = NULL,
    max_cor = NULL,
    max_vif = NULL,
    function_name = NULL,
    quiet = FALSE
){

  if(is.null(function_name)){
    function_name <- "collinear::autoconfig_arg_max_vif()"
  }

  if(
    is.numeric(max_vif) &&
    (max_vif > 10 || max_vif < 1)
  ){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": invalid 'max_vif' value, setting it to NULL to proceed with the autoconfiguration."
      )

    }

    max_cor <- NULL

  }

  if(is.null(max_vif)){

    predictors.numeric <- identify_predictors_numeric(
      df = df,
      predictors = predictors,
      quiet = quiet
    )

    if(is.null(predictors.numeric)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": no numeric variables available, skipping VIF autoconfiguration (max_vif = NULL)."
        )

      }

      return(NULL)

    } else if(length(predictors.numeric) == 1){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one numeric variable available, skipping VIF autoconfiguration (max_vif = NULL)."
        )

      }

      return(NULL)

    }

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": autoconfiguring 'max_vif' ..."
      )

    }

    if(is.null(max_cor)){

      max_cor <- autoconfig_arg_max_cor(
        df = df,
        predictors = predictors.numeric,
        max_cor = max_cor,
        function_name = NULL,
        quiet = TRUE
      )

    }

    if(length(predictors.numeric) > 1){

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

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": 'max_vif' = ",
          max_vif
        )

      }

    } else {
      max_vif <- NULL
    }

  }

  max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  max_vif


}
