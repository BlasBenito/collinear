# Experiment: Correlation vs VIF Equivalence Mapping

# This experiment establishes the relationship between max_cor and max_vif thresholds by running 10,000 iterations comparing cor_select() and vif_select() outputs.
#
# The goal is to find the max_vif threshold that produces the most similar variable selection as a given max_cor threshold, measured by Jaccard similarity.
#
# Results are used to:
# 1. Understand the relationship between `max_cor` and `max_vif`
# 2. Create the gam_cor_to_vif model for automatic threshold conversion
# 3. Generate the lookup table prediction_cor_vif

#dependencies ----
library(collinear)
library(future)
library(future.apply)
library(progressr)
library(dplyr)
library(ggplot2)
library(distantia)
library(mgcv)

#parallelization ----
future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

#simulation parameters ----

##simulated data size
input_columns <- 500
input_rows <- 10000

##total number of repetitions
n_iterations <- 10000

#minimum and maximum number of predictors allowed during subsetting
min_predictors <- 10
max_predictors <- 50

#threshold candidates
max_vif_candidates <- seq(
  from = 1,
  to = 10,
  by = 0.1
  )

max_cor_candidates <- seq(
  from = 0.1,
  to = 0.99,
  by = 0.01
  )

#random seed
seed <- 1

#synthetic data ----
df <- distantia::zoo_simulate(
  name = "sim",
  cols = input_columns,
  rows = input_rows,
  seed = seed,
  seasons = 4,
  independent = FALSE
) |>
  as.data.frame()

#iteration parameters ----
#rows = predictors * 30 to ensure stable VIF estimates
set.seed(seed)
iterations_df <- data.frame(
  max_cor = sample(
    x = max_cor_candidates,
    size = n_iterations,
    replace = TRUE
  ),
  n_cols = sample(
    x = seq(
      from = min_predictors,
      to = max_predictors,
      by = 1
      ),
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

#functions ----

#' Run one iteration of cor vs vif comparison
#' @param i iteration index
#' @param params dataframe with iteration parameters
#' @param data full dataset to subsample from
#' @return dataframe row with results, or NULL if filtering fails
run_iteration <- function(df, n_cols, n_rows, max_cor) {

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

  #correlation-based filtering
  selection_cor <- collinear::cor_select(
    df = df_subset,
    predictors = colnames(df_subset),
    max_cor = max_cor,
    quiet = TRUE
  )

  #find max_vif that produces most similar selection to cor_select by maximizing Jaccard similarity
  best_jaccard <- 0
  best_vif_index <- 1
  best_selection_vif <- character(0)

  for (j in seq_along(max_vif_candidates)) {

    selection_vif <- collinear::vif_select(
      df = df_subset,
      predictors = colnames(df_subset),
      max_vif = max_vif_candidates[j],
      quiet = TRUE
    )

    #jaccard similarity
    jaccard <- length(intersect(selection_vif, selection_cor)) /
      length(union(selection_vif, selection_cor))

    ## Update if this is better
    if (jaccard > best_jaccard) {
      best_jaccard <- jaccard
      best_vif_index <- j
      best_selection_vif <- selection_vif
    }
  }

  #return results using best match
  data.frame(
    input_rows = n_rows,
    input_predictors = n_cols,
    output_predictors = length(best_selection_vif),
    max_cor = max_cor,
    max_vif = max_vif_candidates[best_vif_index],
    out_selection_jaccard = best_jaccard
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
        n_cols =  iterations_df[i, "n_cols"],
        n_rows = iterations_df[i, "n_rows"],
        max_cor = iterations_df[i, "max_cor"]
      )
    },
    future.seed = TRUE
  )
})

#combine results ----
experiment_cor_vs_vif <- dplyr::bind_rows(results_list) |>
  dplyr::arrange(out_selection_jaccard)


#model gam_cor_to_vif ----

#check source for weights
hist(experiment_cor_vs_vif$out_selection_jaccard, breaks = 50)

#optimization
#find suitable k and weight exponent to fit the final model
optimization_df <- expand.grid(
  k = 3:9,
  weight_exp = 2:6
)

#iterate over combinations of k and weight_exp
for(i in seq_len(nrow(optimization_df))){

  m.i <- mgcv::gam(
    formula = max_vif ~ s(max_cor, k = optimization_df[i, "k"]),
    weights = experiment_cor_vs_vif$out_selection_jaccard^optimization_df[i, "weight_exp"],
    data = experiment_cor_vs_vif
  )

  #r-squared
  optimization_df[i, "r_squared"] <- summary(m.i)$r.sq

  #degrees of freedom
  optimization_df[i, "edf"] <- sum(m.i$edf)

}

#select top 90% r_squared and bottom 10% edf
#arrange by r_squared
#select top 1
optimization_df <- optimization_df |>
  dplyr::filter(
    r_squared >= stats::quantile(x = r_squared, probs = 0.90)
  ) |>
  dplyr::filter(
    edf <= stats::quantile(x = edf, probs = 0.10)
  ) |>
  dplyr::arrange(
    dplyr::desc(r_squared)
  ) |>
  dplyr::slice_head(n = 1)

optimization_df


#fit model
gam_cor_to_vif <- mgcv::gam(
  formula = max_vif ~ s(max_cor, k = optimization_df$k),
  weights = experiment_cor_vs_vif$out_selection_jaccard^optimization_df$weight_exp,
  data = experiment_cor_vs_vif
)

summary(gam_cor_to_vif)


#visualization
ggplot2::ggplot(experiment_cor_vs_vif) +
  ggplot2::aes(
    x = max_cor,
    y = max_vif,
    color = out_selection_jaccard
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = as.formula(paste0("y ~ s(x, k = ", optimization_df$k, ")")),
    mapping = ggplot2::aes(weight = out_selection_jaccard^optimization_df[i, "weight_exp"]),
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
    x = "Input max_cor Threshold",
    y = "Equivalent max_vif Threshold"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right"
  )

#dataframe prediction_cor_to_vif ----
prediction_cor_to_vif <- data.frame(
  max_cor = seq(0.10, 1.00, by = 0.001)
)

prediction_cor_to_vif$max_vif <- mgcv::predict.gam(
  object = gam_cor_to_vif,
  newdata = prediction_cor_to_vif
) |>
  round(digits = 3)


#save results ----
usethis::use_data(experiment_cor_vs_vif, overwrite = TRUE)

usethis::use_data(gam_cor_to_vif, overwrite = TRUE)

usethis::use_data(prediction_cor_to_vif, overwrite = TRUE)


#reset parallelization ----
future::plan(sequential)
