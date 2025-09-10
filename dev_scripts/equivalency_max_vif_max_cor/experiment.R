library(collinear)
library(future)
library(tictoc)
library(ggplot2)

future::plan(
  strategy = future::multisession,
  workers = length(future::availableWorkers()) - 2
)

#data to use
data(vi, vi_predictors_numeric)

df <- vi[, vi_predictors_numeric]

#candidate values
max_vif_candidates <- seq(from = 2.5, to = 10, by = 0.1)
max_cor_candidates <- seq(from = 0.5, to = 0.99, by = 0.01)
n_predictors_candidates <- seq(from = 5, to = length(vi_predictors_numeric))
n_rows_candidates <- seq(from = 30 * length(vi_predictors_numeric), to = nrow(df))

#random seed
set.seed(1)

#iterations
n <- 10000
iterations <- seq_len(n)

iterations_df <- data.frame(
  max_cor = sample(x = max_cor_candidates, size = n, replace = TRUE),
  n_predictors = sample(x = n_predictors_candidates, size = n, replace = TRUE),
  n_rows = sample(x = n_rows_candidates, size = n, replace = TRUE),
  out_max_vif = rep(NA, n),
  out_dissimilarity = rep(NA, n)
)

tic()

#run experiment
experiment_list <- future.apply::future_lapply(
  X = iterations,
  FUN = function(i){

    #get params
    iterations.df.i <- iterations_df[i, ]

    #subset data frame
    df.i <- df[
      sample(x = seq_len(nrow(df)), size = iterations.df.i$n_rows),
      sample(x = seq_len(ncol(df)), size = iterations.df.i$n_predictors)
    ]

    #cor
    out.cor <- collinear::cor_select(
      df = df.i,
      predictors = colnames(df.i),
      preference_order = colnames(df.i),
      max_cor = iterations.df.i$max_cor,
      quiet = TRUE
    )

    #vif
    for(j in seq_len(length(max_vif_candidates))){

      out.vif <- collinear::vif_select(
        df = df.i,
        predictors = colnames(df.i),
        preference_order = colnames(df.i),
        max_vif = max_vif_candidates[j],
        quiet = TRUE
      )

      if(length(out.vif) >= length(out.cor)){
        break
      }

    }

    #fill results
    iterations.df.i$out_max_vif <- max_vif_candidates[j]
    iterations.df.i$out_dissimilarity <- length(intersect(out.vif, out.cor)) / length(union(out.vif, out.cor))

    return(iterations.df.i)

  },
  future.seed = TRUE

  )

experiment_df <- dplyr::bind_rows(experiment_list)

toc()

ggplot(
  data = experiment_df |>
    dplyr::filter(
      out_dissimilarity > 0.5
    )
) +
  aes(
    x = max_cor,
    y = out_max_vif,
    color = out_dissimilarity
  ) +
  geom_point() +
  geom_smooth(method = mgcv::gam, formula = y ~ s(x)) +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Equivalency Between Pairwise Correlation and VIF",
    x = "Max Pearson Correlation",
    y = "Maximum VIF",
    color = "Selection\nSimilarity\n(Jaccard)"
  ) +
  theme_bw()


ggplot(
  data = experiment_df
) +
  aes(
    x = max_cor,
    y = out_max_vif,
    color = n_predictors
  ) +
  geom_point() +
  scale_color_viridis_c(option = "turbo") +
  labs(
    title = "Equivalency Between Pairwise Correlation and VIF",
    x = "Max Pearson Correlation",
    y = "Maximum VIF",
    color = "Predictors"
  ) +
  theme_bw()

save(experiment_df, file = "dev_scripts/equivalency_max_vif_max_cor/experiment_results.RData")
