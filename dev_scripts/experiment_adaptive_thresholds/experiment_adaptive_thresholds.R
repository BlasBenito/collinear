# =============================================================================
# Simulation: Validating Adaptive Multicollinearity Thresholds
# =============================================================================
# This experiment validates the sigmoid-based adaptive threshold system in
# collinear() by running 10,000 iterations on random subsets of a synthetic
# dataset with realistic correlation structures.
#
# The goal is to verify that:
# 1. Output VIF stays bounded between ~2.5 and ~7.5 across all input conditions
# 2. The system adapts appropriately to different correlation structures
# 3. Predictor retention scales reasonably with input size
# =============================================================================

# Working directory setup
setwd(here::here())

# Dependencies
library(collinear)
library(future)
library(future.apply)
library(progressr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(distantia)

# Parallelization
future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

# =============================================================================
# Helper Functions
# =============================================================================

#' Extract a specific statistic from collinear_stats() output
#' @param stats Output from collinear_stats()
#' @param m Method: "correlation" or "vif"
#' @param s Statistic name: "quantile_0.75", "maximum", etc.
#' @return Numeric value
get_stat <- function(stats, m, s) {
  stats$value[stats$method == m & stats$statistic == s]
}

#' Run one iteration of the adaptive threshold experiment
#' @param df Source dataframe with all predictors
#' @param n_predictors Number of predictors to sample
#' @return One-row dataframe with input/output statistics, or NULL if skipped
run_iteration <- function(df, n_predictors) {

  # Random subset of predictors
  df_subset <- df[, sample(ncol(df), n_predictors), drop = FALSE]

  # Validate and clean

  df_subset <- collinear::validate_arg_df(
    df = df_subset,
    predictors = colnames(df_subset),
    quiet = TRUE
  )

  # Input statistics
  input_stats <- collinear::collinear_stats(
    df = df_subset,
    quiet = TRUE
  )

  input_vif_max <- get_stat(input_stats, "vif", "maximum")
  input_cor_q75 <- get_stat(input_stats, "correlation", "quantile_0.75")

  # Skip low-multicollinearity cases
  # These don't meaningfully test the adaptive system
  if (input_cor_q75 < 0) {
    return(NULL)
  }

  # Apply adaptive filtering
  result <- collinear::collinear(
    df = df_subset,
    predictors = colnames(df_subset),
    quiet = TRUE
  )

  # Output statistics
  output_stats <- collinear::collinear_stats(
    df = result$result$df,
    predictors = result$result$selection,
    quiet = TRUE
  )

  # Return results
  data.frame(
    input_predictors = n_predictors,
    output_predictors = length(result$result$selection),
    input_cor_q75 = input_cor_q75,
    output_cor_q75 = get_stat(output_stats, "correlation", "quantile_0.75"),
    input_cor_max = get_stat(input_stats, "correlation", "maximum"),
    output_cor_max = get_stat(output_stats, "correlation", "maximum"),
    input_vif_max = input_vif_max,
    output_vif_max = get_stat(output_stats, "vif", "maximum")
  )

}

# =============================================================================
# Synthetic Dataset
# =============================================================================

# Parameters
seed <- 1
set.seed(seed)

# Generate correlated time series with realistic structure
# distantia::zoo_simulate creates seasonal patterns with inter-column correlation
df <- distantia::zoo_simulate(
  name = "sim",
  cols = 500,
  rows = 3000,
  seed = seed,
  seasons = 0,
  independent = FALSE
) |>
  as.data.frame()

# =============================================================================
# Run Experiment
# =============================================================================

# Simulation parameters
n_iterations <- 10000
min_predictors <- 10
max_predictors <- 100

# Random predictor counts for each iteration
n_predictors_sample <- sample(
  x = min_predictors:max_predictors,
  size = n_iterations,
  replace = TRUE
)

# Progress bar
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")

# Parallel execution
progressr::with_progress({

  p <- progressr::progressor(steps = n_iterations)

  results_list <- future.apply::future_lapply(
    X = n_predictors_sample,
    FUN = function(n_preds) {
      p()
      run_iteration(
        df = df,
        n_predictors = n_preds
        )
    },
    future.seed = TRUE
  )

})

# =============================================================================
# Process Results
# =============================================================================

experiment_adaptive_thresholds <- results_list |>
  dplyr::bind_rows() |>
  dplyr::arrange(output_vif_max)

# Summary statistics
cat("Iterations completed:", nrow(experiment_adaptive_thresholds), "\n")
cat("Output VIF range:",
    round(min(experiment_adaptive_thresholds$output_vif_max), 2), "-",
    round(max(experiment_adaptive_thresholds$output_vif_max), 2), "\n")
cat("Mean predictor retention:",
    round(mean(experiment_adaptive_thresholds$output_predictors /
                 experiment_adaptive_thresholds$input_predictors) * 100, 1), "%\n")

# =============================================================================
# Visualization
# =============================================================================

# Left panel: Adaptive threshold effectiveness
# Shows output VIF bounded between ~2.5 and ~7.5 across correlation structures
p1 <- ggplot2::ggplot(experiment_adaptive_thresholds) +
  ggplot2::aes(
    x = input_cor_q75,
    y = output_vif_max,
    color = input_vif_max
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray30") +
  ggplot2::annotate(
    "text",
    x = 0.1,
    y = 2.65,
    label = "VIF = 2.5",
    hjust = 1,
    size = 3.5,
    color = "gray30"
  ) +
  ggplot2::scale_color_viridis_c(option = "turbo") +
  ggplot2::labs(
    title = "Adaptive Threshold Effectiveness",
    x = "Input Correlation (quantile 0.75)",
    y = "Output Maximum VIF",
    color = "Input\nPredictors"
  ) +
  ggplot2::theme_bw()

# Right panel: Predictor retention
# Shows filtering scales reasonably with input size
p2 <- ggplot(experiment_adaptive_thresholds) +
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

# Combined figure
p1 | p2

# =============================================================================
# Export
# =============================================================================

future::plan(sequential)
usethis::use_data(experiment_adaptive_thresholds, overwrite = TRUE)
