#' Pairwise Correlation Stats
#'
#' Computes the the minimum, mean, maximum, and quantiles 0.05, 0.25, median (0.5), 0.75, and 0.95 of the column "correlation" in the output of [cor_df()].
#'
#' @inheritParams cor_matrix
#'
#' @returns
#' \itemize{
#'   \item if \code{df} is the output of [cor_df()]: stats dataframe with the columns \code{statistic} and \code{correlation}.
#'   \item if \code{df} is a data frame with predictors: list with:
#'   \itemize{
#'     \item \code{correlation}: result from [cor_df()].
#'     \item \code{stats} data frame.
#'   }
#' }
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
#'
#' #OPTIONAL: parallelization setup
#' # only worth it for large data
#' # future::plan(
#' #   future::multisession,
#' #   workers = 2
#' # )
#' #
#' #OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #returns correlation and stats dataframes
#' stats <- cor_stats(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric[1:10]
#' )
#'
#' #vector of correlation values
#' head(stats$correlation)
#'
#' #stats data frame
#' stats$stats
#'
#' #using output of cor_df
#' df_correlation <- cor_df(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric[1:10]
#' )
#'
#' #returns stats dataframe only
#' stats <- cor_stats(
#'   df = df_correlation
#' )
#'
#' stats
#'
#' #OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @export
cor_stats <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_stats()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #cor_df data frame
  if(
    all(
      names(df) %in% c(
        "x",
        "y",
        "correlation",
        "metric"
      )
    )
  ){

    correlation_df <- df
    rm(df)

    out_type <- "df"

  } else {

    #data frame of predictors
    correlation_df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    out_type <- "list"

  }


  values <- stats::na.omit(correlation_df$correlation)

  if(length(values) < 10){

    stop(
      "\n",
      function_name,
      ": not enough correlation values to compute meaningful stats.",
      call. = FALSE
    )

  }

  if(length(values) < 30){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": correlation stats were computed with fewer than 30 cases, interpret them with care."
      )

    }

  }

  stats <- c(
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

    "mean" = mean(values),

    "maximum" = max(values)

  ) |>
    round(digits = 4) |>
    sort()

  stats_names <- names(stats)
  names(stats) <- NULL

  stats_df <- data.frame(
    statistic = stats_names,
    correlation = stats
  )

  if(out_type == "list"){
    out <- list(
      correlation = correlation_df,
      stats = stats_df
    )
  } else {
    out <- stats_df
  }

  out

}
