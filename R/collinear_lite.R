#' @title Minimal Wrapper for \code{collinear}
#' @description
#' Minimalistic version of [collinear()] with the following features:
#' \itemize{
#'   \item Argument \code{response} instead of \code{responses}, to process only one response at a time.
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
#'   response = "vi_numeric",
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
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear_lite()",
    ... = ...
  )

  if(
    !is.null(response) &&
    length(response) > 1
    ){

    response <- response[1]

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' must be of length 1, using the first element with value '",
        response,
        "'."
      )

    }

  }

  #call to collinear
  out <- collinear(
    df = df,
    responses = response,
    predictors = predictors,
    encoding_method = NULL,
    preference_order = preference_order,
    f = NULL,
    max_cor = max_cor,
    max_vif = max_vif,
    quiet = quiet,
    function_name = function_name
  )

  if(!is.null(response) && response %in% names(out)){
    out <- out[[response]]$selection
  } else {
    out <- out$result$selection
  }

  out

}
