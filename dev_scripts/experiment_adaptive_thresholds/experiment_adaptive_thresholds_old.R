if(basename(getwd()) != "experiment_adaptive_thresholds"){
  setwd(here::here())
  setwd("dev_scripts/experiment_adaptive_thresholds")
}

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
ncols <- 100
nrows <- 3000

#simulation
n <- 10000
iterations <- seq_len(n)
min_cols <- 4
max_cols <- 100
seed <- 2

#synthetic dataset
x1 <- distantia::zoo_simulate(
  name = "sim",
  cols = ncols,
  rows = nrows,
  seed = seed,
  seasons = 2,
  independent = FALSE
) |>
  as.data.frame()

df <- dplyr::bind_cols(x1, x1, x1, x1, x1)


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
  input_cor_q75 = rep(NA, n),
  output_cor_q75 = rep(NA, n),
  input_cor_max = rep(NA, n),
  output_cor_max = rep(NA, n),
  input_vif_max = rep(NA, n),
  output_vif_max = rep(NA, n)
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
      df.i <- df[,
        sample(x = seq_len(ncol(df)), size = iterations.df.i$input_predictors)
      ]

      df.i <- validate_arg_df(
        df = df.i,
        predictors = colnames(df.i),
        quiet = TRUE
      )

      input_stats <- collinear_stats(
        df = df.i,
        quiet = TRUE
      )

      iterations.df.i$input_cor_q75 <- input_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "quantile_0.75"
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
        df = selection$result$df,
        predictors = selection$result$selection
      )

      iterations.df.i$output_cor_q75 <- output_stats |>
        dplyr::filter(
          method == "correlation",
          statistic == "quantile_0.75"
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

      if(iterations.df.i$output_vif_max > 10){

        save(df.i, file = paste0("wrong_data_", i, ".RData"))

      }

      iterations.df.i$output_predictors <- length(selection$result$selection)

      return(iterations.df.i)

    },
    future.seed = TRUE

  )

})


#join rows
experiment_adaptive_thresholds <- experiment_list |>
  dplyr::bind_rows() |>
  dplyr::select(
    input_predictors,
    output_predictors,
    input_cor_q75,
    output_cor_q75,
    input_cor_max,
    output_cor_max,
    input_vif_max,
    output_vif_max
  ) |>
  dplyr::arrange(
    output_vif_max
    )


experiment_adaptive_thresholds |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_cor_q75,
    y = output_vif_max
  ) +
  ggplot2::geom_point()


mean(experiment_adaptive_thresholds$input_cor_q75)

cor_plot <- experiment_adaptive_thresholds |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_cor_q75,
    y = output_vif_max,
    color = (output_predictors/input_predictors)*100
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::scale_color_viridis_c(option = "turbo", direction = 1) +
  ggplot2::labs(
    title = "Input Correlation vs Output VIF",
    x = "Input Correlation (quantile 0.75)",
    y = "Output Maximum VIF",
    color = "Selected\nPredictors\n(%)"
  ) +
  ggplot2::theme_bw()

preds_plot <- experiment_adaptive_thresholds |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_predictors,
    y = output_predictors,
    color = output_vif_max
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(method = "gam", formula = y ~ s(x, k = 3), color = "gray50", se = TRUE) +
  ggplot2::scale_color_viridis_c(option = "turbo", direction = 1) +
  ggplot2::labs(
    title = "Input vs. Selected Predictors",
    x = "Input Predictors",
    y = "Output Predictors",
    color = "Output\nmax VIF"
  ) +
  ggplot2::scale_y_continuous(breaks = c(0, 5, 10, 15, 20)) +
  ggplot2::theme_bw()

cor_plot | preds_plot


#reset future backend (also kills hanging processes)
future::plan(sequential)


usethis::use_data(experiment_adaptive_thresholds, overwrite = TRUE)
