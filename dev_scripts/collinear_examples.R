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
#  only numerics filtered by VIF
x <- collinear(
  df = df,
  predictors = predictors,
  max_cor = 0.75, #default
  max_vif = 5     #default
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

x

#disabling target encoding
# x <- collinear(
#   df = vi,
#   response = "vi_numeric",
#   predictors = predictors_mixed,
#   encoding_method = NULL
# )
#
# x

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

x

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

x

#with binomial response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
#  all predictors filtered by correlation and VIF
x <- collinear(
  df = df,
  response = "vi_binomial",
  predictors = predictors
)

x


#with counts response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
#  all predictors filtered by correlation and VIF
x <- collinear(
  df = df,
  response = "vi_counts",
  predictors = predictors
)

x

#with categorical response
#--------------------------------

#  target encoding
#  automated preference order (different f function)
# all predictors filtered by correlation
# numeric predictors filtered by VIF
x <- collinear(
  df = df,
  response = "vi_category",
  predictors = predictors
)

x
