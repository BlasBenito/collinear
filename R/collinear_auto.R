#' @title Automated Multicollinearity Management
#'
#' @description
#'
#' A fire-and-forget wrapper to [collinear()] to automatically manage multicollinearity without user input.
#'
#' It configures target encoding, preference order, and the maximum multicollinearity levels (\code{max_vif} and \code{max_cor}) automatically.
#'
#' Target encoding is triggered with the method "loo" (leave-one-out, see [target_encoding_lab()] for further details) when a response is numeric and there are categorical predictors.
#'
#' Preference order is computed automatically when \code{responses} is not NULL. The function [f_auto()] is used to select a proper method depending on the response and predictor types.
#'
#' Configures \code{max_cor} as \code{max(x, 0.58)}, where \code{x} is the median correlation between all predictors as returned by [cor_stats()]. The minimum correlation \code{0.58} roughly matches a VIF of 2.5.
#'
#' Configures \code{max_vif} as the prediction of the model [gam_cor_to_vif] on the given \code{max_cor} value obtained as explained above.
#'
#' In the experiments performed during development (see [experiment_collinear_auto]), the function reduced the total number of predictors by 68%, with a range between 80% for 100 initial predictors and 44% for 25 predictors.
#'
#' The average difference in median correlation between the input and selected predictors was -0.16, and the resulting maximum VIF across all selections was 2.75, with an average of 2.3.
#'
#' These results indicate that the function offers a robust functionality with a minimal setup.
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
    quiet = FALSE,
    ...
){

  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear_auto()",
    function_name = dots$function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  responses <- validate_arg_responses(
    df = df,
    responses = responses,
    quiet = quiet,
    function_name = function_name
  )

  predictors <- validate_arg_predictors(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  df.ncol <- ncol(df)

  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #revalidate predictors if columns were removed
  if(ncol(df) < df.ncol){

    attributes(responses)$validated <- NULL
    attributes(predictors)$validated <- NULL

    responses <- validate_arg_responses(
      df = df,
      responses = responses,
      quiet = quiet,
      function_name = function_name
    )

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  #computing cor df and m here to avoid recomputation in collinear()
  cor.df <- cor_df(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  m <- cor_matrix(
    df = cor.df,
    quiet = quiet,
    function_name = function_name
  )

  #autoconfiguration
  cor.stats <- cor_stats(
    df = cor.df,
    predictors = predictors,
    quiet = TRUE,
    function_name = function_name
  )

  cor.median <- cor.stats[
    cor.stats$statistic == "median",
    "value"
  ]

  max_cor <- max(
    0.58, #approx matches VIF = 2.5
    round(x = cor.median, digits = 2)
  )

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": setting 'max_cor' to ",
      max_cor,
      "."
    )

    if(max_cor > 0.90){

      message(
        "\n",
        function_name,
        ": the median correlation in this dataset is very high, automated multicollinearity filtering might yield suboptimal results in this case."
      )

    }

  }

  max_vif <- mgcv::predict.gam(
    object = collinear::gam_cor_to_vif,
    newdata = data.frame(
      max_cor = max_cor
    )
  ) |>
    round(digits = 2)

  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": setting 'max_vif' to ",
      max_vif,
      "."
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
    function_name = function_name,
    m = m
  )

  out

}
