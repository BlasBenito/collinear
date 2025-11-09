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

#data to use
data(vi, vi_predictors_numeric)
df <- vi[, vi_predictors_numeric]
rm(vi)

#add synthetic data

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

df <- cbind(df, x, y)
rm(x)

#max dimensions
min_cols <- 10
max_cols <- 100
max_rows <- 10000


#candidate values
n_cols_candidates <- seq(from = min_cols, to = max_cols)

#random seed
set.seed(1)

#iterations
n <- 10000
iterations <- seq_len(n)

iterations_df <- data.frame(
  input_predictors = sample(x = n_cols_candidates, size = n, replace = TRUE),
  output_predictors = rep(NA, n),
  input_cor_median = rep(NA, n),
  output_cor_median = rep(NA, n),
  input_vif_max = rep(NA, n),
  output_vif_max = rep(NA, n)
)

#compute n_rows (minimum of 30 per column)
iterations_df$n_rows <- vapply(
  iterations_df$input_predictors,
  function(nc) sample(seq(nc * 30, nrow(df)), 1),
  numeric(1)
)


#run experiment
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")
progressr::with_progress({

  p <- progressr::progressor(along = iterations)

  experiment_list <- future.apply::future_lapply(
    X = iterations,
    FUN = function(i){

      p()

      #get params
      iterations.df.i <- iterations_df[i, ]

      #subset data frame
      df.i <- df[
        sample(x = seq_len(nrow(df)), size = iterations.df.i$n_rows),
        sample(x = seq_len(ncol(df)), size = iterations.df.i$input_predictors)
      ]

      df.i <- validate_arg_df(
        df = df.i,
        predictors = colnames(df.i),
        quiet = TRUE
      )

      input_stats <- collinear_stats(
        df = df.i
      )

      iterations.df.i$input_cor_median <- input_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "median"
        ) |>
        dplyr::pull(
          value
        )

      iterations.df.i$input_vif_max <- input_stats |>
        dplyr::filter(
          method == "vif",
          statistic == "maximum"
        ) |>
        dplyr::pull(
          value
        )

      #cor
      selection <- collinear::collinear_auto(
        df = df.i,
        predictors = colnames(df.i),
        quiet = TRUE
      )

      #output stats
      output_stats <- collinear_stats(
        df = df.i,
        predictors = selection$result$selection
      )

      iterations.df.i$output_cor_median <- output_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "median"
        ) |>
        dplyr::pull(
          value
        )

      iterations.df.i$output_vif_max <- output_stats |>
        dplyr::filter(
          method == "vif",
          statistic == "maximum"
        ) |>
        dplyr::pull(
          value
        )

      iterations.df.i$output_predictors <- length(selection$result$selection)

      return(iterations.df.i)

    },
    future.seed = TRUE

  )

})


#reset future backend (also kills hanging processes)
future::plan(sequential)

#join rows
experiment_df <- dplyr::bind_rows(experiment_list)
rm(experiment_list)

experiment_df <- experiment_df |>
  dplyr::mutate(
    predictors_diff_percent = 100 - ((output_predictors * 100)/input_predictors),
    correlation_diff = input_cor_median - output_cor_median,
    vif_diff = input_vif_max - output_vif_max
  )

mean(experiment_df$predictors_diff_percent)
mean(experiment_df$correlation_diff)
mean(experiment_df$output_vif_max)

ggplot(experiment_df) +
  aes(
    x = input_cor_median,
    y = output_cor_median,
    color = predictors_diff_percent
  ) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_smooth()


ggplot(experiment_df) +
  aes(
    x = input_predictors,
    y = output_predictors,
    color = input_cor_median
  ) +
  geom_point() +
  geom_abline(slope = 1) +
  geom_smooth()

experiment_collinear_auto <- experiment_df |>
  dplyr::transmute(
    input_rows = n_rows,
    input_predictors,
    output_predictors,
    input_cor_median = round(input_cor_median, 2),
    output_cor_median = round(output_cor_median, 2),
    input_vif_max = round(input_vif_max, 2),
    output_vif_max = round(output_vif_max, 2)
  )

usethis::use_data(experiment_collinear_auto)
