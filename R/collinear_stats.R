#' Multicollinearity Stats
#'
#' Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25, median (0.5), 0.75, and 0.95 of the correlations and variance inflation factors in a given dataframe. Wraps the functions [cor_stats()] and [vif_stats()]
#'
#' @inheritParams cor_matrix
#' @returns dataframe with columns \code{method} (with values "correlation" and "vif"), \code{statistic} and \code{value}
#'
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
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
#' x <- collinear_stats(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric
#' )
#'
#' x
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @export
collinear_stats <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear_stats()",
    function_name = dots$function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(!inherits(x = df, what = "collinear_cor_df")){

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    ncol.df <- ncol(df)

    df <- validate_arg_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    if(ncol.df > ncol(df)){

      attributes(predictors)$validated <- NULL

      predictors <- validate_arg_predictors(
        df = df,
        predictors = predictors,
        quiet = quiet,
        function_name = function_name
      )

    }

    cor.df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  } else {

    cor.df <- df
    predictors <- unique(
      c(cor.df$x, cor.df$y)
    )

  }


  cor.m <- cor_matrix(
    df = cor.df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name,
    m = dots$m
  )

  cor.stats <- cor_stats(
    df = cor.df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  cor.vif <- vif_stats(
    df = cor.df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name,
    m = cor.m
  )

  out <- rbind(
    cor.stats,
    cor.vif
  )

  out

}
