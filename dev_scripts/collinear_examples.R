#parallelization setup
future::plan(
  future::multisession,
  workers = 3 #set to parallelly::availableCores() - 1
)

#progress bar
progressr::handlers(global = TRUE)

#subset to limit example run time
df <- vi[1:1000, ]
predictors <- vi_predictors[1:10]

#predictors has mixed types
sapply(
  X = df[, predictors, drop = FALSE],
  FUN = class
)

#without response
#--------------------------------
#  no target encoding
#  no preference order
#  all predictors filtered by correlation
#  VIF filtering not required
x <- collinear(
  df = df,
  predictors = predictors,
  max_cor = 0.75, #default
  max_vif = 5    #default
  )

x

#all correlations below max_cor
cor_df(
  df = df,
  predictors = x
)

#all vif below max vif
#ignores categoricals
vif_df(
  df = df,
  predictors = x
)


#VIF filtering only
#--------------------------------
#  no target encoding
#  no preference order
#  only numerics filtered by VIF
x <- collinear(
  df = df,
  predictors = predictors,
  max_cor = NULL
)

#correlation filtering only
#--------------------------------
#  no target encoding
#  no preference order
#  all predictors filtered by correlation
x <- collinear(
  df = df,
  predictors = predictors,
  max_vif = NULL
)


#with numeric response
#--------------------------------

#  target encoding
#  automated preference order
#  all predictors filtered by correlation and VIF
x <- collinear(
  df = df,
  response = "vi_numeric",
  predictors = predictors
)

#disabling target encoding
#commented because it is much slower
# x <- collinear(
#   df = df,
#   response = "vi_numeric",
#   predictors = predictors,
#   encoding_method = NULL
# )

#with custom preference order
x <- collinear(
  df = df,
  response = "vi_numeric",
  predictors = predictors,
  preference_order = c(
    "swi_mean",
    "soil_type",
    "koppen_zone"
  )
)

#with quantitative preference order
preference_df <- preference_order(
  df = df,
  response = "vi_numeric",
  predictors = predictors
)

x <- collinear(
  df = df,
  response = "vi_numeric",
  predictors = predictors,
  preference_order = preference_df
)


#with binomial response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
#  all predictors filtered by correlation and VIF
# x <- collinear(
#   df = df,
#   response = "vi_binomial",
#   predictors = predictors
# )



#with counts response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
#  all predictors filtered by correlation and VIF
# x <- collinear(
#   df = df,
#   response = "vi_counts",
#   predictors = predictors
# )

#with categorical response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
# all predictors filtered by correlation
# numeric predictors filtered by VIF
# x <- collinear(
#   df = df,
#   response = "vi_categorical",
#   predictors = predictors
# )


#resetting to sequential processing
future::plan(future::sequential)

