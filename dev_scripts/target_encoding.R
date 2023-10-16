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
  sd_width = c(0.01, 0.1, 1),
  noise = c(0, 1)
)
