library(collinear)
library(future)
library(tictoc)
library(ggplot2)
library(patchwork)
library(distantia)
library(progressr)
library(mgcv)

future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

#PARAMS

#simulated datasets
ncols <- 50
nrows <- 3000

#simulation
n <- 100
iterations <- seq_len(n)
min_cols <- 3
max_cols <- 30
max_rows <- 3000
seed <- 1

#synthetic dataset
#with seasons
x1 <- distantia::zoo_simulate(
  name = "sim",
  cols = ncols,
  rows = nrows,
  seed = seed,
  seasons = 2,
  independent = FALSE
) |>
  as.data.frame()

colnames(x1) <- paste0("x1_", colnames(x1))

#duplicate columns
x2 <- x1

colnames(x2) <- paste0("x2_", colnames(x2))

#without seasons
y <- distantia::zoo_simulate(
  name = "sim",
  cols = ncols,
  rows = nrows,
  seed = seed + 1,
  seasons = 0,
  independent = TRUE
) |>
  as.data.frame()

colnames(y) <- paste0("y_", colnames(y))

df <- cbind(x, y)


#candidate values
iterations_df <- data.frame(
  input_predictors = sample(
    x = seq(
      from = min_cols,
      to = max_cols
      ),
    size = n,
    replace = TRUE
    ),
  output_predictors = rep(NA, n),
  input_cor_median = rep(NA, n),
  output_cor_median = rep(NA, n),
  input_cor_max = rep(NA, n),
  output_cor_max = rep(NA, n),
  input_vif_max = rep(NA, n),
  output_vif_max = rep(NA, n)
)

#compute n_rows (minimum of 30 per column)
iterations_df$n_rows <- vapply(
  iterations_df$input_predictors,
  function(nc) sample(seq(nc * 10, nrow(df)), 1),
  numeric(1)
)


#run experiment
set.seed(seed)
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

      iterations.df.i$input_cor_max <- input_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "maximum"
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

      #avoid working with non-collinear data
      if(iterations.df.i$input_vif_max <= 2.5){return(NULL)}

      #cor
      selection <- collinear::collinear(
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

      iterations.df.i$output_cor_max <- output_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "maximum"
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


#join rows
experiment_adaptive_thresholds <- experiment_list |>
  dplyr::bind_rows() |>
  dplyr::transmute(
    input_rows = n_rows,
    input_predictors,
    selected_predictors = output_predictors,
    input_cor_median = round(input_cor_median, 2),
    output_cor_median = round(output_cor_median, 2),
    input_cor_max = round(input_cor_max, 2),
    output_cor_max = round(output_cor_max, 2),
    input_vif_max = round(input_vif_max, 2),
    output_vif_max = round(output_vif_max, 2)
  )

hist(experiment_adaptive_thresholds$input_cor_median)

plot(
  experiment_adaptive_thresholds$input_cor_median,
  experiment_adaptive_thresholds$input_predictors
)

mean(experiment_adaptive_thresholds$input_cor_median)

cor_plot <- experiment_adaptive_thresholds |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_cor_median,
    y = output_cor_median,
    color = output_vif_max
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_abline(slope = 1) +
  ggplot2::scale_color_viridis_c(option = "turbo") +
  ggplot2::coord_fixed(
    xlim = c(0, 0.65),
    ylim = c(0, 0.65)
  ) +
  ggplot2::labs(
    title = "Input vs. Output Correlation",
    x = "Input Median Correlation",
    y = "Output Median Correlation",
    color = "Output\nmax VIF"
  ) +
  ggplot2::theme_bw()

preds_plot <- experiment_adaptive_thresholds |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_predictors,
    y = selected_predictors,
    color = output_vif_max
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_abline(slope = 1) +
  ggplot2::scale_color_viridis_c(option = "turbo") +
  ggplot2::labs(
    title = "Input vs. Selected Predictors",
    x = "Input Predictors",
    y = "Selected Predictors",
    color = "Output\nmax VIF"
  ) +
  ggplot2::theme_bw()

cor_plot | preds_plot


#reset future backend (also kills hanging processes)
future::plan(sequential)


usethis::use_data(experiment_adaptive_thresholds, overwrite = TRUE)
