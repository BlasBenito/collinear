load("data/ecoregions.rda")
load("data/ecoregions_predictors.rda")
devtools::load_all()

numeric <- ecoregions$plant_richness

character <- ecoregions$primary_productivity

numeric[c(1, 5, 8)] <- NA
character[c(1, 16, 34)] <- NA

x <- target_encode(
  numeric = numeric,
  character = character
)





#development arguments
df <- ecoregions
predictors <- ecoregions_predictors
min_numerics <- 1
decimals <- 4

df <- df_inspect(
  df = df,
  min_rows = 30
  )

predictors <- predictors_inspect(
  df = df,
  predictors = predictors,
  min_numerics = 1,
  decimals = decimals
)

#cor_df
##################################
cor_df(
  df = ecoregions,
  predictors = ecoregions_predictors
)

