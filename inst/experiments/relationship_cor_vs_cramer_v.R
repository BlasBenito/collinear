# Experiment: Pearson Correlation vs Cramer's V Comparison

# This experiment explores the relationship between Pearson correlation and
# Cramer's V across different cardinalities (number of categories) by running
# 1,000 iterations on synthetic paired integer vectors.
#
# The goal is to understand:
# 1. How Cramer's V relates to Pearson correlation for discretized numeric data
# 2. How cardinality affects this relationship
# 3. Whether Cramer's V systematically over- or under-estimates association

# dependencies ----
library(collinear)
library(distantia)
library(ggplot2)

# simulation parameters ----

# total number of repetitions
n_iterations <- 1000

# range of sample sizes
min_rows <- 30
max_rows <- 1000

# cardinality levels to test (number of unique categories)
cardinality_candidates <- seq(
  from = 2,
  to = 10,
  by = 2
)

# random seed
seed <- 1

# iteration parameters ----
set.seed(seed)

iterations_df <- data.frame(
  n_rows = sample(
    x = min_rows:max_rows,
    size = n_iterations,
    replace = TRUE
  ),
  cardinality = sample(
    x = cardinality_candidates,
    size = n_iterations,
    replace = TRUE
  ),
  out_cramer_v = NA_real_,
  out_pearson_cor = NA_real_
)

# functions ----

#' Run one iteration of correlation vs Cramer's V comparison
#' @param n_rows number of rows to simulate
#' @param cardinality number of unique integer values
#' @return named list with Pearson correlation and Cramer's V
run_iteration <- function(n_rows, cardinality) {
  # simulate paired integer vectors
  df_sim <- distantia::zoo_simulate(
    rows = n_rows,
    time_range = c(1, n_rows),
    cols = 2,
    data_range = c(0, cardinality),
    irregular = FALSE
  ) |>
    as.matrix()

  x <- as.integer(df_sim[, 1])
  y <- as.integer(df_sim[, 2])

  # compute metrics
  pearson_cor <- stats::cor(
    x = x,
    y = y
  ) |>
    abs()

  cramer_v <- collinear::cor_cramer(
    x = x,
    y = y
  )

  list(
    out_pearson_cor = pearson_cor,
    out_cramer_v = cramer_v
  )
}

# run iterations ----
for (i in seq_len(nrow(iterations_df))) {
  result_i <- run_iteration(
    n_rows = iterations_df[i, "n_rows"],
    cardinality = iterations_df[i, "cardinality"]
  )

  iterations_df[i, "out_pearson_cor"] <- result_i$out_pearson_cor
  iterations_df[i, "out_cramer_v"] <- result_i$out_cramer_v
}

# combine results ----
experiment_cor_vs_cramer <- iterations_df |>
  dplyr::arrange(cardinality)

# visualization ----
ggplot2::ggplot(experiment_cor_vs_cramer) +
  ggplot2::aes(
    x = out_pearson_cor,
    y = out_cramer_v,
    group = factor(cardinality),
    color = factor(cardinality)
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 3),
    se = TRUE
  ) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "gray30"
  ) +
  ggplot2::scale_color_viridis_d(
    option = "turbo",
    name = "Cardinality"
  ) +
  ggplot2::labs(
    title = "Pearson Correlation vs Cramer's V",
    subtitle = "Across different cardinality levels",
    x = "Pearson Correlation (absolute)",
    y = "Cramer's V"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right"
  )
