
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

preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_rsquared,
  workers = 2
)

preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_lm_coef,
  workers = 2
)

preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_gam_deviance,
  workers = 2
)

preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors[1:10],
  f = f_rf_deviance,
  workers = 2
)

selected.variables <- collinear(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = preference.order$predictor
)
