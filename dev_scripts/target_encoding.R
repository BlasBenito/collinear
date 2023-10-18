library(collinear)

data(vi, vi_predictors)

output <- target_encoding_lab(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  methods = c(
    "mean",
    "rank",
    "rnorm",
    "loo"
  ),
  rnorm_sd_multiplier = c(0.01, 0.1, 1),
  white_noise = c(0, 1)
)
