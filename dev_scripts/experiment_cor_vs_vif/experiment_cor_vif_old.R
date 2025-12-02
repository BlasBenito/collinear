library(collinear)
library(future)
library(tictoc)
library(ggplot2)
library(distantia)
library(progressr)
library(mgcv)

future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

#PARAMS
#max dimensions
min_cols <- 4
max_cols <- 100
max_rows <- 3000

#candidate values
max_vif_candidates <- seq(from = 1, to = 10, by = 0.1)
max_cor_candidates <- seq(from = 0.1, to = 0.99, by = 0.01)
input_predictors_candidates <- seq(from = min_cols, to = max_cols)

#random seed
set.seed(1)

#iterations
n <- 10000
iterations <- seq_len(n)


#DATA
data(vi, vi_predictors_numeric)
df <- vi[, vi_predictors_numeric]
rm(vi)


#with seasons
x <- distantia::zoo_simulate(
  name = "sim",
  cols = 100,
  rows = nrow(df),
  seasons = floor(nrow(df)/1000),
  seed = 1
) |>
  as.data.frame()

#without seasons
y <- distantia::zoo_simulate(
  name = "sim",
  cols = 100,
  rows = nrow(df),
  seed = 2
) |>
  as.data.frame()

df <- dplyr::bind_cols(df, x, y)

rm(x)

#iterations dataframe
iterations_df <- data.frame(
  max_cor = sample(x = max_cor_candidates, size = n, replace = TRUE),
  input_predictors = sample(x = input_predictors_candidates, size = n, replace = TRUE),
  max_vif = rep(NA, n),
  selection_similarity = rep(NA, n),
  output_predictors = rep(NA, n)
)

#compute input_rows (minimum of 30 per column)
iterations_df$input_rows <- vapply(
  iterations_df$input_predictors,
  function(nc) sample(seq(nc * 30, max_rows), 1),
  numeric(1)
)


tic()

#run experiment
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")
progressr::with_progress({

  p <- progressor(along = iterations)

  experiment_list <- future.apply::future_lapply(
    X = iterations,
    FUN = function(i){

      p()

      #get params
      iterations.df.i <- iterations_df[i, ]

      #subset data frame
      df.i <- df[
        sample(x = seq_len(nrow(df)), size = iterations.df.i$input_rows),
        sample(x = seq_len(ncol(df)), size = iterations.df.i$input_predictors)
      ]

      #cor
      out.cor <- collinear::cor_select(
        df = df.i,
        predictors = colnames(df.i),
        preference_order = colnames(df.i),
        max_cor = iterations.df.i$max_cor,
        quiet = TRUE
      )

      #if out.cor selects nothing, cancel this run
      if(length(out.cor) == length(colnames(df.i))){
        return(NULL)
      }

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
      iterations.df.i$max_vif <- max_vif_candidates[j]
      iterations.df.i$selection_similarity <- length(intersect(out.vif, out.cor)) / length(union(out.vif, out.cor))
      iterations.df.i$output_predictors <- length(out.vif)

      return(iterations.df.i)

    },
    future.seed = TRUE

  )

})

toc()

#join rows
experiment_df <- dplyr::bind_rows(experiment_list)

#plot experiment
experiment_df |>
  dplyr::arrange(
    selection_similarity
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = max_cor,
    y = max_vif,
    color = selection_similarity,
    weight = selection_similarity^3
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 9),
    method.args = list(
      select = TRUE
    ),
    color = "black",
    se = TRUE
  ) +
  ggplot2::scale_color_viridis_c(
    option = "turbo",
    direction = 1
  ) +
  ggplot2::labs(
    title = "Experiment Pearson Correlation vs. VIF",
    x = "Input max_cor",
    y = "max_vif leading to most similar selection",
    color = "Jaccard\nsimilarity\nbetween\nselections"
  ) +
  ggplot2::theme_bw()


save(experiment_df, file = "dev_scripts/experiment_cor_vs_vif/experiment_results.RData")

experiment_cor_vs_vif <- experiment_df
usethis::use_data(experiment_cor_vs_vif, overwrite = TRUE)


gam_cor_to_vif <- mgcv::gam(
  formula = max_vif ~ s(max_cor, k = 9),
  weights = experiment_df$selection_similarity^3,
  data = experiment_df,
  select = TRUE
)

usethis::use_data(gam_cor_to_vif, overwrite = TRUE)

#equivalency table
prediction_cor_to_vif <- data.frame(
  max_cor = seq(0.1, 1, by = 0.01),
  max_vif = NA
)

prediction_cor_to_vif$max_vif <- mgcv::predict.gam(
  object = gam_cor_to_vif,
  newdata = prediction_cor_to_vif) |>
  round(digits = 2)

usethis::use_data(prediction_cor_to_vif, overwrite = TRUE)

#reset future backend (also kills hanging processes)
future::plan(sequential)
