#' Pairwise Correlation Stats
#'
#' Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25, median (0.5), 0.75, and 0.95 of the column "correlation" in the output of [cor_df()].
#'
#' @inheritParams collinear
#' @returns dataframe with columns \code{statistic} and \code{value}
#'
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
#'
#' # ## OPTIONAL: parallelization setup
#' # ## irrelevant when all predictors are numeric
#' # ## only worth it for large data with many categoricals
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' # ## OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' x <- vif_stats(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric
#' )
#'
#' x
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family multicollinearity_assessment
#' @export
vif_stats <- function(
  df = NULL,
  predictors = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif_stats()",
    function_name = dots$function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #cor_df dataframe
  if (!"collinear_vif_df" %in% class(df)) {
    df <- vif_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name,
      m = dots$m
    )
  }

  values <- stats::na.omit(df$vif)

  stats <- c(
    "n" = length(values),

    "minimum" = min(values),

    "quantile_0.05" = stats::quantile(
      x = values,
      probs = 0.05,
      names = FALSE
    ),

    "quantile_0.25" = stats::quantile(
      x = values,
      probs = 0.25,
      names = FALSE
    ),

    "mean" = mean(values),

    "median" = stats::median(
      x = values
    ),

    "quantile_0.75" = stats::quantile(
      x = values,
      probs = 0.75,
      names = FALSE
    ),

    "quantile_0.95" = stats::quantile(
      x = values,
      probs = 0.95,
      names = FALSE
    ),

    "maximum" = max(values)
  ) |>
    round(digits = 4)

  stats_names <- names(stats)
  names(stats) <- NULL

  out <- data.frame(
    method = "vif",
    statistic = stats_names,
    value = stats
  )

  out
}
