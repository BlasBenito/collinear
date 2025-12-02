# =============================================================================
# Experiment: Correlation vs VIF Equivalence Mapping
# =============================================================================
# This experiment establishes the relationship between max_cor and max_vif
# thresholds by running 10,000 iterations comparing cor_select() and vif_select()
# outputs.
#
# The goal is to find the max_vif threshold that produces the most similar
# variable selection as a given max_cor threshold, measured by Jaccard similarity.
#
# Results are used to:
# 1. Create the gam_cor_to_vif model for automatic threshold conversion
# 2. Generate the equivalence_cor_vif lookup table
# 3. Validate that both filtering methods produce comparable results
# =============================================================================

# Dependencies ----------------------------------------------------------------
library(collinear)
library(future)
library(future.apply)
library(progressr)
library(dplyr)
library(ggplot2)
library(distantia)
library(mgcv)

# Simulation Parameters -------------------------------------------------------

##simulated data size
input_columns <- 500
input_rows <- 10000

## Data dimensions
min_predictors <- 4
max_predictors <- 100
max_rows <- 3000

## Threshold candidates to test
max_vif_candidates <- seq(from = 1, to = 10, by = 0.1)
max_cor_candidates <- seq(from = 0.1, to = 0.99, by = 0.01)

## Number of iterations
n_iterations <- 1000

## Random seed
seed <- 1
set.seed(seed)

# Generate Synthetic Dataset --------------------------------------------------

# ## Load base data
# data(vi, vi_predictors_numeric)
# df_base <- vi[, vi_predictors_numeric]
#
# ## Add synthetic columns with realistic correlation structures
# df_seasonal <- distantia::zoo_simulate(
#   name = "seasonal",
#   cols = 100,
#   rows = nrow(df_base),
#   seasons = floor(nrow(df_base) / 1000),
#   seed = 1
# ) |>
#   as.data.frame()
#
# df_random <- distantia::zoo_simulate(
#   name = "random",
#   cols = 100,
#   rows = nrow(df_base),
#   seasons = 0,
#   seed = 2
# ) |>
#   as.data.frame()
#
# ## Combine all predictors
# df <- dplyr::bind_cols(df_base, df_seasonal, df_random)

#synthetic data
df_dependent <- distantia::zoo_simulate(
  name = "sim",
  cols = input_columns,
  rows = input_rows,
  seed = seed,
  seasons = 0,
  independent = FALSE
) |>
  as.data.frame()

df_independent <- distantia::zoo_simulate(
  name = "sim",
  cols = input_columns,
  rows = input_rows,
  seed = seed,
  seasons = 0,
  independent = TRUE
) |>
  as.data.frame()

df <- dplyr::bind_cols(
  df_dependent,
  df_independent
)

## Clean up
rm(vi, df_base, df_seasonal, df_random)

# Setup Iteration Parameters --------------------------------------------------

iterations_df <- data.frame(
  iteration = seq_len(n_iterations),
  max_cor = sample(
    x = max_cor_candidates,
    size = n_iterations,
    replace = TRUE
  ),
  input_predictors = sample(
    x = seq(min_predictors, max_predictors),
    size = n_iterations,
    replace = TRUE
  )
)

## Compute sample sizes (minimum 30 rows per predictor for stable VIF)
iterations_df$input_rows <- vapply(
  X = iterations_df$input_predictors,
  FUN = function(n_cols) {
    sample(
      x = seq(n_cols * 30, max_rows),
      size = 1
    )
  },
  FUN.VALUE = numeric(1)
)

# Core Experiment Function ----------------------------------------------------

#' Run one iteration of cor vs vif comparison
#' @param i iteration index
#' @param params dataframe with iteration parameters
#' @param data full dataset to subsample from
#' @return dataframe row with results, or NULL if filtering fails
run_iteration <- function(i, params, data) {

  ## Extract parameters
  params_i <- params[i, ]

  ## Random subset of data
  df_subset <- data[
    sample(nrow(data), params_i$input_rows),
    sample(ncol(data), params_i$input_predictors),
    drop = FALSE
  ]

  ## Run correlation-based filtering
  selection_cor <- collinear::cor_select(
    df = df_subset,
    predictors = colnames(df_subset),
    preference_order = colnames(df_subset),
    max_cor = params_i$max_cor,
    quiet = TRUE
  )

  ## Skip if cor_select didn't remove anything
  if (length(selection_cor) == ncol(df_subset)) {
    return(NULL)
  }

  ## Find max_vif that produces most similar selection to cor_select
  ## by maximizing Jaccard similarity
  best_jaccard <- 0
  best_vif_index <- 1  # will always be updated
  best_selection_vif <- character(0)

  for (j in seq_along(max_vif_candidates)) {

    selection_vif <- collinear::vif_select(
      df = df_subset,
      predictors = colnames(df_subset),
      preference_order = colnames(df_subset),
      max_vif = max_vif_candidates[j],
      quiet = TRUE
    )

    ## Compute Jaccard similarity for this max_vif
    jaccard <- length(intersect(selection_vif, selection_cor)) /
      length(union(selection_vif, selection_cor))

    ## Update if this is better
    if (jaccard > best_jaccard) {
      best_jaccard <- jaccard
      best_vif_index <- j
      best_selection_vif <- selection_vif
    }
  }

  ## Return results using best match
  data.frame(
    iteration = i,
    input_rows = params_i$input_rows,
    input_predictors = params_i$input_predictors,
    output_predictors = length(best_selection_vif),
    input_max_cor = params_i$max_cor,
    output_max_vif = max_vif_candidates[best_vif_index],
    out_selection_jaccard = best_jaccard
  )
}

# Run Experiment in Parallel --------------------------------------------------

## Setup parallelization
future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

## Setup progress bar
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")

## Run iterations
message("\nRunning ", n_iterations, " iterations...")
progressr::with_progress({

  p <- progressr::progressor(steps = n_iterations)

  results_list <- future.apply::future_lapply(
    X = seq_len(n_iterations),
    FUN = function(i) {
      p()
      run_iteration(
        i = i,
        params = iterations_df,
        data = df
      )
    },
    future.seed = TRUE
  )
})

## Combine results
experiment_cor_vs_vif <- dplyr::bind_rows(results_list) |>
  dplyr::arrange(out_selection_jaccard)

message("Completed ", nrow(experiment_cor_vs_vif), " successful iterations")

# Visualization ---------------------------------------------------------------

p <- ggplot2::ggplot(experiment_cor_vs_vif) +
  ggplot2::aes(
    x = input_max_cor,
    y = output_max_vif,
    color = out_selection_jaccard,
    weight = out_selection_jaccard^3
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 9),
    method.args = list(select = TRUE),
    color = "black",
    se = TRUE
  ) +
  ggplot2::scale_color_viridis_c(
    option = "turbo",
    direction = 1,
    name = "Jaccard\nSimilarity"
  ) +
  ggplot2::labs(
    title = "Correlation vs VIF Threshold Equivalence",
    subtitle = paste0("Based on ", nrow(experiment_cor_vs_vif), " successful iterations"),
    x = "Input max_cor Threshold",
    y = "Equivalent max_vif Threshold"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right"
  )

print(p)

# Fit GAM Model for Threshold Conversion -------------------------------------

## Fit weighted GAM (higher weight for better matching selections)
gam_cor_to_vif <- mgcv::gam(
  formula = output_max_vif ~ s(input_max_cor, k = 9),
  weights = experiment_cor_vs_vif$out_selection_jaccard^3,
  data = experiment_cor_vs_vif,
  select = TRUE
)

## Model summary
message("\n=== GAM Model Summary ===")
print(summary(gam_cor_to_vif))

# Generate Equivalence Lookup Table -------------------------------------------

equivalence_cor_vif <- data.frame(
  max_cor = seq(0.10, 1.00, by = 0.01)
)

equivalence_cor_vif$max_vif <- mgcv::predict.gam(
  object = gam_cor_to_vif,
  newdata = equivalence_cor_vif
) |>
  round(digits = 2)

## Display sample
message("\n=== Equivalence Table (first 10 rows) ===")
print(head(equivalence_cor_vif, 10))

# Export Results --------------------------------------------------------------

## Save experiment results
usethis::use_data(experiment_cor_vs_vif, overwrite = TRUE)
message("\nExported: experiment_cor_vs_vif")

## Save GAM model
usethis::use_data(gam_cor_to_vif, overwrite = TRUE)
message("Exported: gam_cor_to_vif")

## Save equivalence table
usethis::use_data(equivalence_cor_vif, overwrite = TRUE)
message("Exported: equivalence_cor_vif")

# Cleanup ---------------------------------------------------------------------

## Reset parallelization
future::plan(sequential)

message("\n=== Experiment Complete ===")
