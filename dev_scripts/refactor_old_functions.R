
df <- cor_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

m <- cor_matrix(
  df = df
)

v <- vif_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

selected.variables <- cor_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

selected.variables <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

selected.variables <- collinear(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)
