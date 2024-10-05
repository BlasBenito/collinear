data(
  vi,
  vi_predictors
)

#subset to limit example run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:10]

#vi_predictors has mixed types
sapply(
  X = vi[, vi_predictors],
  FUN = class
)

#without response
#--------------------------------
#  no target encoding
#  no preference order
#  correlation analysis includes categoricals
# vif analysis ignores categoricals
x <- collinear(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.75, #default
  max_vif = 5     #default
  )

x

#all correlations below max_cor
cor_df(
  df = vi,
  predictors = x
)

#all vif below max vif
#ignores categoricals
vif_df(
  df = vi,
  predictors = x
)


#with numeric response
#--------------------------------

#target encoding transforms categoricals to numeric
#all variables go through VIF analysis
#automated preference order
x <- collinear(
  df = vi,
  response = "vi_numeric",
  predictors = vi_predictors
)

x

#disabled target encoding
#only numeric variables go through VIF analysis
#automated preference order
x <- collinear(
  df = vi,
  response = "vi_numeric",
  predictors = vi_predictors,
  encoding_method = NULL
)

x





#with response
#without preference_order
#restrictive max_cor and max_vif
#numerics and categorical variables in output
selected.predictors <- collinear(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  max_cor = 0.5,
  max_vif = 2.5
)

selected.predictors

#with response
#with user-defined preference_order
#restrictive max_cor and max_vif
#numerics and categorical variables in output
selected.predictors <- collinear(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = c(
    "soil_temperature_mean",
    "swi_mean",
    "rainfall_mean",
    "evapotranspiration_mean"
  ),
  max_cor = 0.5,
  max_vif = 2.5
)

selected.predictors


#with response
#with automated preference_order
#restrictive max_cor and max_vif
#numerics and categorical variables in output
preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_rsquared, #cor(response, predictor)
)

selected.predictors <- collinear(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = preference.order,
  max_cor = 0.5,
  max_vif = 2.5
)

selected.predictors
