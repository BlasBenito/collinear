#' Pairwise Correlation Stats
#'
#' Generates a correlation dataframe with [cor_df()] and computes the the minimum, mean, maximum, and quantiles 0.05, 0.25, 0.50, 0.75, and 0.95 of the Pearson correlations (and Cramer's V if there is more than one categorical predictor).
#'
#' @inheritParams cor_matrix
#'
#' @returns list:
#' \itemize{
#'   \item values: numeric vector of absolute correlations.
#'   \item stats: dataframe with the columns \code{statistic} and \code{value}.
#' }
#' @examples
#' stats <- cor_stats(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric
#' )
#'
#' head(stats$values)
#' str(stats$stats)
#' @autoglobal
#' @family pairwise_correlation
#' @export
cor_stats <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE
){

  if(
    all(
      names(df) %in% c(
        "x",
        "y",
        "correlation"
      )
    ) == FALSE
  ){


    correlation_df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = quiet
    )

  } else {

    correlation_df <- df
    rm(df)

  }


  values <- na.omit(correlation_df$correlation)

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

    "quantile_0.50" = stats::quantile(
      x = values,
      probs = 0.50,
      names = FALSE
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
    value = stats
  )

  if(quiet == FALSE){
    print(stats_df, row.names = FALSE, right = FALSE)
  }

  out <- list(
    values = values,
    stats = stats_df
  )

  out

}
