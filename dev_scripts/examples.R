library(collinear)

#collinear (done and tested)
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#reduce size of vi to speed-up example execution
vi <- vi[1:1000, ]

#no response
#no preference_order
#permissive max_cor and max_vif
#only numeric variables in output
selected.predictors <- collinear(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.8,
  max_vif = 10
  )

selected.predictors

#no response
#no preference_order
#restrictive max_cor and max_vif
#only numeric variables in output
selected.predictors <- collinear(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.5,
  max_vif = 2.5
)

selected.predictors

#with response
#no preference_order
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
  workers = 1
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


#cor_select (done and tested)
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#reduce size of vi to speed-up example execution
vi <- vi[1:1000, ]

#no response
#no preference_order
#permissive max_cor
selected.predictors <- cor_select(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.8
)

selected.predictors

#no response
#no preference_order
#restrictive max_cor
selected.predictors <- cor_select(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.5
)

selected.predictors

#with response
#no preference_order
#restrictive max_cor
#slightly different solution than previous one
#because here target encoding is done against the response
#while before was done pairwise against each numeric predictor
selected.predictors <- cor_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  max_cor = 0.5
)

selected.predictors

#with response
#with user-defined preference_order
#restrictive max_cor
#numerics and categorical variables in output
selected.predictors <- cor_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = c(
    "soil_type", #categorical variable
    "soil_temperature_mean",
    "swi_mean",
    "rainfall_mean",
    "evapotranspiration_mean"
  ),
  max_cor = 0.5
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
  f = f_rsquared #cor(response, predictor)
)

head(preference.order)

selected.predictors <- cor_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = preference.order,
  max_cor = 0.5
)

selected.predictors


#vif_select (done and tested)
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#reduce size of vi to speed-up example execution
vi <- vi[1:1000, ]

#no response
#no preference_order
#permissive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 10
)

selected.predictors

#no response
#no preference_order
#restrictive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 2.5
)

selected.predictors

#with response
#no preference_order
#restrictive max_cor
#slightly different solution than previous one
#because categorical variables are target-enccoded
selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  max_vif = 2.5
)

selected.predictors

#with response
#with user-defined preference_order
#restrictive max_cor
#numerics and categorical variables in output
selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = c(
    "soil_type", #categorical variable
    "soil_temperature_mean",
    "swi_mean",
    "rainfall_mean",
    "evapotranspiration_mean"
  ),
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
  f = f_rsquared #cor(response, predictor)
)

head(preference.order)

selected.predictors <- vif_select(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  preference_order = preference.order,
  max_vif = 2.5
)

selected.predictors


#target_encoding_lab()
###################################################
###################################################
###################################################
#loading example data
data(
  vi,
  vi_predictors
  )

#reduce size of vi to speed-up example runtime
vi <- vi[1:1000, ]

#applying all methods for a continuous response
df <- target_encoding_lab(
  df = vi,
  response = "vi_mean",
  predictors = "koppen_zone",
  encoding_methods = c(
    "mean",
    "rank",
    "rnorm",
    "loo"
  ),
  rnorm_sd_multiplier = c(0, 0.1, 0.2),
  white_noise = c(0, 0.1, 0.2)
)

#identify encoded predictors
predictors.encoded <- grep(
  pattern = "*__encoded*",
  x = colnames(df),
  value = TRUE
)

#correlation between encoded predictors and the response
stats::cor(
  x = df[["vi_mean"]],
  y = df[, predictors.encoded],
  use = "pairwise.complete.obs"
)

#plot encoded predictors vs response
par(mfrow = c(4, 3))
x <- lapply(
  X = predictors.encoded,
  FUN = function(x) plot(
    x = df[[x]],
    y = df$vi_mean,
    xlab = x,
    ylab = "vi_mean"
    )
)


#target_encoding_methods
###################################################
###################################################
###################################################
#loading example data
data(vi)

#mean encoding
#-------------

#without noise
df <- target_encoding_mean(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)

#with noise
df <- target_encoding_mean(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  white_noise = 0.1,
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)


#group rank
#----------

df <- target_encoding_rank(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)


#leave-one-out
#-------------

#without noise
df <- target_encoding_loo(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)

#with noise
df <- target_encoding_loo(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  white_noise = 0.1,
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)


#rnorm
#-----

#without sd multiplier
df <- target_encoding_rnorm(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)

#with sd multiplier
df <- target_encoding_rnorm(
  df = vi,
  response = "vi_mean",
  predictor = "soil_type",
  rnorm_sd_multiplier = 0.1,
  replace = TRUE
)

plot(
  x = df$soil_type,
  y = df$vi_mean,
  xlab = "encoded variable",
  ylab = "response"
)
