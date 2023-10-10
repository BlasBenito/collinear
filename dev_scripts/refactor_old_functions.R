load("data/ecoregions.rda")
load("data/ecoregions_predictors.rda")
devtools::load_all()

#cor_df
##################################
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

