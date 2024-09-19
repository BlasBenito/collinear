library(collinear)

#collinear DONE
#cor_df DONE
#cor_matrix DONE
#cor_select DONE
#cramer_v DONE
#target_encoding_lab DONE
#target_encoding_methods DONE
#vif_select DONE
#preference_order DONE
#f_rsquared DONE
#f_gam_deviance DONE
#f_rf_rsquared DONE
#identify_numeric_predictors DONE
# dentify_non_numeric_predictors DONE
#identify_zero_variance_predictors DONE
#validate_df DONE
#validate_predictors DONE
#validate_response DONE
#vif_df DONE


#validate_response DONE
###################################################
###################################################
###################################################

data(
  vi
)

#validating example data frame
vi <- validate_df(
  df = vi
)

#validating example predictors
response <- validate_response(
  df = vi,
  response = "vi_mean"
)

#tagged as validated
attributes(response)$validated

#validate_predictors DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
  )

#validating example data frame
vi <- validate_df(
  df = vi
)

#validating example predictors
vi_predictors <- validate_predictors(
  df = vi,
  predictors = vi_predictors
)

#tagged as validated
attributes(vi_predictors)$validated

#validate_df DONE
###################################################
###################################################
###################################################

data(vi)

#validating example data frame
vi <- validate_df(
  df = vi
)

#tagged as validated
attributes(vi)$validated

#identify_zero_variance_predictors DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#create zero variance predictors
vi$zv_1 <- 1
vi$zv_2 <- runif(n = nrow(vi), min = 0, max = 0.0001)


#add to vi predictors
vi_predictors <- c(
  vi_predictors,
  "zv_1",
  "zv_2"
)

#identify zero variance predictors
zero.variance.predictors <- identify_zero_variance_predictors(
  df = vi,
  predictors = vi_predictors
)

zero.variance.predictors

#identify_non_numeric_predictors DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

non.numeric.predictors <- identify_non_numeric_predictors(
  df = vi,
  predictors = vi_predictors
)

non.numeric.predictors


#identify_numeric_predictors DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

numeric.predictors <- identify_numeric_predictors(
  df = vi,
  predictors = vi_predictors
)

numeric.predictors

#f_rf_rsquared DONE
###################################################
###################################################
###################################################
data(vi)

#subset to limit run time
vi <- vi[1:1000, ]

#this example requires "ranger" installed in the system
if(requireNamespace(package = "ranger", quietly = TRUE)){

  f_rf_rsquared(
    x = "growing_season_length", #predictor
    y = "vi_mean",               #response
    df = vi
  )

}


#f_gam_deviance DONE
###################################################
###################################################
###################################################
data(vi)

#subset to limit run time
vi <- vi[1:1000, ]

#this example requires "mgcv" installed in the system
if(requireNamespace(package = "mgcv", quietly = TRUE)){

  f_gam_deviance(
    x = "growing_season_length", #predictor
    y = "vi_mean",               #response
    df = vi
  )

}


#f_rsquared DONE
###################################################
###################################################
###################################################
data(vi)

#subset to limit run time
vi <- vi[1:1000, ]

f_rsquared(
  x = "growing_season_length", #predictor
  y = "vi_mean",               #response
  df = vi
)


#preference_order DONE
###################################################
###################################################
###################################################
data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]

#computing preference order
#with response
#numeric and categorical predictors in the output
#as the R-squared between each predictor and the response
preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_rsquared
  )

preference.order

#using it in variable selection with collinear()
selected.predictors <- cor_select(
  df = vi,
  response = "vi_mean", #don't forget the response!
  predictors = vi_predictors,
  preference_order = preference.order,
  max_cor = 0.75
  )

selected.predictors

#check their correlations
selected.predictors.cor <- cor_df(
  df = vi,
  response = "vi_mean",
  predictors = selected.predictors
)

#all correlations below max_cor
selected.predictors.cor

#USING A CUSTOM FUNCTION
#custom function to compute RMSE between a predictor and a response
#x is a predictor name
#y is a response name
#df is a data frame with multiple predictors and one response
#must return a single number, with higher number indicating higher preference
#notice we use "one minus RMSE" to give higher rank to variables with lower RMSE
f_rmse <- function(x, y, df){

  xy <- df[, c(x, y)] |>
    na.omit() |>
    scale()

  1 - sqrt(mean((xy[, 1] - xy[, 2])^2))

}

preference.order <- preference_order(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors,
  f = f_rmse
)

preference.order



#cramer_vi DONE
###################################################
###################################################
###################################################
#loading example data
data(vi)

#subset to limit run time
vi <- vi[1:1000, ]

#computing Cramer's V for two categorical predictors
v <- cramer_v(
  x = vi$soil_type,
  y = vi$koppen_zone
  )

v


#collinear DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]

#without response
#without preference_order
#permissive max_cor and max_vif
#only numeric variables in output
selected.predictors <- collinear(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.8,
  max_vif = 10
  )

selected.predictors

#without response
#without preference_order
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
  f = f_rsquared #cor(response, predictor)
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


#cor_select DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:10]

#without response
#without preference_order
#permissive max_cor
selected.predictors <- cor_select(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.8
)

selected.predictors

#without response
#without preference_order
#restrictive max_cor
selected.predictors <- cor_select(
  df = vi,
  predictors = vi_predictors,
  max_cor = 0.5
)

selected.predictors

#with response
#without preference_order
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


#cor_df DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:10]

#without response
#categorical vs categorical compared with cramer_v()
#categorical vs numerical compared wit stats::cor() via target-encoding
#numerical vs numerical compared with stats::cor()
df <- cor_df(
  df = vi,
  predictors = vi_predictors
)

head(df)

#with response
#different solution than previous one
#because target encoding is done against the response
#rather than against the other numeric in the pair
df <- cor_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

head(df)

#vif_df DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]

#without response
#only numeric predictors are returned
df <- vif_df(
  df = vi,
  predictors = vi_predictors
)

df

#with response
#categorical and numeric predictors are returned
df <- vif_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

df


#cor_matrix DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:5]

#convert correlation data frame to matrix
df <- cor_df(
  df = vi,
  predictors = vi_predictors
)

m <- cor_matrix(
  df = df
)

#show first three columns and rows
#pairs of categoricals
m[1:5, 1:5]

#generate correlation matrix directly
m <- cor_matrix(
  df = vi,
  predictors = vi_predictors
)

m[1:5, 1:5]

#with response (much faster)
#different solution than previous one
#because target encoding is done against the response
#rather than against the other numeric in the pair
m <- cor_matrix(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

m[1:5, 1:5]



#vif_select DONE
###################################################
###################################################
###################################################

data(
  vi,
  vi_predictors
)

#subset to limit run time
vi <- vi[1:1000, ]
vi_predictors <- vi_predictors[1:10]

#without response
#without preference_order
#permissive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 10
)

selected.predictors

#without response
#without preference_order
#restrictive max_vif
#only numeric predictors are processed
selected.predictors <- vif_select(
  df = vi,
  predictors = vi_predictors,
  max_vif = 2.5
)

selected.predictors

#with response
#without preference_order
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


#target_encoding_lab() DONE
###################################################
###################################################
###################################################
#loading example data
data(
  vi,
  vi_predictors
  )

#subset to limit run time
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

#record the user's graphical parameters
user.par <- par(no.readonly = TRUE)

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

#reset the user's graphical parameters
par(user.par)

#target_encoding_methods DONE
###################################################
###################################################
###################################################
#loading example data
data(vi)

#subset to limit run time
vi <- vi[1:1000, ]

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
