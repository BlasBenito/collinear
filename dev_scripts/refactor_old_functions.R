load("data/ecoregions.rda")
load("data/ecoregions_predictors.rda")
devtools::load_all()

#predictors_inspect

#cor_df
##################################
cor_df(
  data = ecoregions,
  predictors = ecoregions_predictors,
  verbose = TRUE
)

