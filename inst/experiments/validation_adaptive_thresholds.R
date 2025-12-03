# Experiment: Validating Adaptive Multicollinearity Thresholds

# This experiment validates the sigmoid-based adaptive threshold system in
# collinear() by running 10,000 iterations on random subsets of a synthetic
# dataset with realistic correlation structures.
#
# The goal is to verify that:
# 1. Output VIF stays bounded between ~2.5 and ~7.5 across most input conditions
# 2. The system adapts appropriately to different correlation structures
# 3. Predictor retention scales reasonably with input size

#dependencies ----
library(collinear)
library(future)
library(future.apply)
library(progressr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(distantia)

# parallelization ----
future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

# simulation parameters ----

#simulated data size
input_columns <- 500
input_rows <- 10000

#total number of repetitions
n_iterations <- 10000

#minimum and maximum number of predictors allowed during subsetting
min_predictors <- 10
max_predictors <- 100

#random seed
seed <- 1

#synthetic data ----
df <- distantia::zoo_simulate(
  name = "sim",
  cols = input_columns,
  rows = input_rows,
  seed = seed,
  seasons = 0,
  independent = FALSE
) |>
  as.data.frame()

#iteration parameters ----
#rows = predictors * 30 to ensure stable VIF estimates
set.seed(seed)
iterations_df <- data.frame(
  n_cols = sample(
    x = min_predictors:max_predictors,
    size = n_iterations,
    replace = TRUE
  )
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    n_rows = n_cols * sample(
      x = 30:100,
      size = 1
    )
  ) |>
  dplyr::ungroup() |>
  as.data.frame()

# functions ----

#' Extract multicollinearity stats
#' @param stats output from collinear_stats()
#' @param m method: "correlation" or "vif"
#' @param s statistic name: "quantile_0.75", "maximum", etc.
#' @return Numeric value
get_stat <- function(stats, m, s) {
  stats$value[stats$method == m & stats$statistic == s]
}

#' Run one experiment iteration
#' @param df full dataframe with predictors
#' @param n_cols number of columns to subset
#' @param n_rows number of rows to subset
#' @return dataframe with multicollinearity stats
run_iteration <- function(df, n_cols, n_rows) {

  #random subset of rows and predictors
  df_subset <- df[
    sample(nrow(df), n_rows),
    sample(ncol(df), n_cols),
    drop = FALSE
  ]

  #validate (to avoid double validation)
  df_subset <- collinear::validate_arg_df(
    df = df_subset,
    predictors = colnames(df_subset),
    quiet = TRUE
  )

  #input stats
  input_stats <- collinear::collinear_stats(
    df = df_subset,
    quiet = TRUE
  )

  #multicollinearity filtering
  result <- collinear::collinear(
    df = df_subset,
    predictors = colnames(df_subset),
    quiet = TRUE
  )

  #output statistics
  output_stats <- collinear::collinear_stats(
    df = result$result$df,
    predictors = result$result$selection,
    quiet = TRUE
  )

  #results dataframe
  data.frame(
    input_rows = n_rows,
    input_predictors = n_cols,
    output_predictors = length(result$result$selection),
    input_cor_q75 = get_stat(
      input_stats,
      "correlation",
      "quantile_0.75"
    ),
    output_cor_q75 = get_stat(
      output_stats,
      "correlation",
      "quantile_0.75"
    ),
    input_cor_max = get_stat(
      input_stats,
      "correlation",
      "maximum"
    ),
    output_cor_max = get_stat(
      output_stats,
      "correlation",
      "maximum"
    ),
    input_vif_max = get_stat(
      input_stats,
      "vif",
      "maximum"
    ),
    output_vif_max = get_stat(
      output_stats,
      "vif",
      "maximum"
    )
  )

}



#progress bar ----
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")
progressr::with_progress({

  p <- progressr::progressor(steps = n_iterations)

#run iterations in parallel ----
  results_list <- future.apply::future_lapply(
    X = seq_len(n_iterations),
    FUN = function(i) {
      p()
      run_iteration(
        df = df,
        n_cols = iterations_df$n_cols[i],
        n_rows = iterations_df$n_rows[i]
      )
    },
    future.seed = TRUE
  )

})

#combine results ----
experiment_adaptive_thresholds <- results_list |>
  dplyr::bind_rows() |>
  dplyr::arrange(output_vif_max)

#visualization ----
#left panel: shows output VIF across correlation structures
p1 <- ggplot2::ggplot(experiment_adaptive_thresholds) +
  ggplot2::aes(
    x = input_cor_q75,
    y = output_vif_max,
    color = input_predictors
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray30") +
  ggplot2::geom_hline(yintercept = 7.5, linetype = "dashed", color = "gray30") +
  ggplot2::annotate(
    "text",
    x = min(experiment_adaptive_thresholds$input_cor_q75),
    y = 7.65,
    label = "VIF = 7.5",
    hjust = 0,
    size = 3.5,
    color = "gray30"
  ) +
  ggplot2::annotate(
    "text",
    x = min(experiment_adaptive_thresholds$input_cor_q75),
    y = 2.65,
    label = "VIF = 2.5",
    hjust = 0,
    size = 3.5,
    color = "gray30"
  ) +
  ggplot2::scale_color_viridis_c(option = "turbo") +
  ggplot2::scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), limits = c(1, 8)) +
  ggplot2::labs(
    title = "Adaptive Threshold Effectiveness",
    x = "Input Correlation (quantile 0.75)",
    y = "Output Maximum VIF",
    color = "Input\nPredictors"
  ) +
  ggplot2::theme_bw()

#predictor retention: shows filtering scales reasonably with input size
p2 <- ggplot2::ggplot(experiment_adaptive_thresholds) +
  ggplot2::aes(
    x = input_predictors,
    y = output_predictors,
    color = output_vif_max
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 3),
    color = "gray30",
    se = TRUE
  ) +
  ggplot2::scale_color_viridis_c(option = "turbo") +
  ggplot2::scale_y_continuous(breaks = seq(0, 20, 5)) +
  ggplot2::labs(
    title = "Predictor Retention",
    x = "Input Predictors",
    y = "Selected Predictors",
    color = "Output\nmax VIF"
  ) +
  ggplot2::theme_bw()

#combine panels
p1 | p2


#save results ----
usethis::use_data(experiment_adaptive_thresholds, overwrite = TRUE)

#reset parallelization ----
future::plan(sequential)
