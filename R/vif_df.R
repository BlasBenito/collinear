#' @title Data Frame with Variance Inflation Factors
#'
#' @description
#'
#' Applies [vif()] to compute the Variance Inflation Factors of a set of numeric predictors.
#'
#' @inheritSection collinear Variance Inflation Factors
#'
#' @inheritParams collinear
#' @return data frame; predictors names and their Variance Inflation Factors
#'
#' @examples
#'
#' data(vi, vi_predictors_numeric)
#'
#' v <- vif_df(
#'   df = vi[1:1000, ],
#'   predictors = vi_predictors_numeric[1:5]
#' )
#'
#' v
#'
#' @autoglobal
#' @family vif
#' @inherit vif_select references
#' @export
vif_df <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif_df()",
    ... = ...
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  df <- validate_arg_df(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  predictors <- validate_arg_predictors_vif(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  #if no predictors
  if(length(predictors) == 0){

    return(
      data.frame(
        variable = character(),
        vif = numeric()
      )
    )

  }

  if(length(predictors) == 1){

    return(
      data.frame(
        variable = predictors,
        vif = 0
      )
    )

  }

  #compute correlation matrix
  m <- cor_matrix(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  out <- vif(
    m = m,
    function_name = function_name
    ) |>
    data.frame(stringsAsFactors = FALSE)

  #format data frame
  colnames(out) <- "vif"
  out$predictor <- colnames(m)
  rownames(out) <- NULL

  out

}





