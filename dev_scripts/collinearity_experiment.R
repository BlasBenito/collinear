library(collinear)
library(dplyr)
library(ranger)
library(mgcv)
library(xgboost)
library(glmnet)
library(nlme)

#example data frame from collinear
data(vi)

#create new dataset from VI
#soil_clay and humidity_range have no correlation
set.seed(1)
df <- vi |>
  dplyr::slice_sample(n = 2000) |>
  dplyr::transmute(
    a = soil_clay,
    b = humidity_range
  ) |>
  scale() |>
  as.data.frame() |>
  dplyr::mutate(
    y = a + b * 0.25 + runif(n = dplyr::n(), min = -0.5, max = 0.5),
    c = a + runif(n = dplyr::n(), min = -0.5, max = 0.5),
    d = b + runif(n = dplyr::n(), min = -0.5, max = 0.5)
  )

#now we have a data frame in which
#y is the response
#a and b are not correlated
#c and d are not correlated
#a is correlated with c
#b is correlated with d
cor_df(
  df = df,
  predictors = c("a", "b", "c", "d")
  )

#with these predictors, we have one model with no multicollinearity:
#y ~ a + b
#and a model with a high multicollinearity
#y ~ a + b + c + d

#separate df train and df test
train <- sample(x = seq_len(nrow(df)), size = 1500)
df_train <- df[train, ]
df_test <- df[-train, ]



#lm
############################################

#without multicollinearity
lm_1 <- lm(
  formula = y ~ a + b,
  data = df_train
  )

#coefficients
round(coefficients(lm_1), 4)

#correlation between predictions and observations
cor(
  x = df_test$y,
  y = predict(lm_1, df_test)
)

#with multicollinearity
lm_2 <- lm(
  formula = y ~ a + b + c + d,
  data = df_train
  )

#notice coefficients of c and d
round(coefficients(lm_2), 4)

#same predictive power though
cor(
  x = df_test$y,
  y = predict(lm_2, df_test)
)

#with gls
###################################
#without multicollinearity
gls_1 <- nlme::gls(
  model = y ~ a + b,
  data = df_train
)

#coefficients
round(coefficients(gls_1), 4)

#correlation between predictions and observations
cor(
  x = df_test$y,
  y = predict(gls_1, df_test)
)

#with multicollinearity
gls_2 <- nlme::gls(
  model = y ~ a + b + c + d,
  data = df_train
)

#notice coefficients of c and d
round(coefficients(gls_2), 4)

#same predictive power though
cor(
  x = df_test$y,
  y = predict(gls_2, df_test)
)


#with glmnet
###################################

#without multicollinearity
glmnet_1 <- glmnet::glmnet(
  x = as.matrix(df_train[, c("a", "b")]),
  y = df_train$y,
  alpha = 1 #lasso penalty
)

coef(glmnet_1, s = 0.01)

cor(
  x = df_test$y,
  y = predict(glmnet_1, newx = as.matrix(df_test[, c("a", "b")]), s = 0.01)
)

#with multicollinearity
glmnet_2 <- glmnet::glmnet(
  x = as.matrix(df_train[, c("a", "b", "c", "d")]),
  y = df_train$y,
  alpha = 1 #lasso penalty
)

coef(glmnet_2, s = 0.01)

cor(
  x = df_test$y,
  y = predict(glmnet_2, newx = as.matrix(df_test[, c("a", "b", "c", "d")]), s = 0.01)
)

#with random forest
###################################

#without multicollinearity
rf_1 <- ranger::ranger(
  formula = y ~ a + b,
  data = df_train,
  importance = "permutation"
)

rf_1$variable.importance

cor(
  x = df_test$y,
  y = predict(rf_1, df_test)$predictions
)

#with multicollinearity
rf_2 <- ranger::ranger(
  formula = y ~ a + b + c + d,
  data = df_train,
  importance = "permutation"
)

rf_2$variable.importance

cor(
  x = df_test$y,
  y = predict(rf_2, df_test)$predictions
)


#with xgboost
##############################
#without multicollinearity
xgboost_1 <- xgboost(
  data = as.matrix(df_train[, c("a", "b")]),
  label = df_train$y,
  objective = "reg:squarederror",
  nrounds = 100,
  verbose = FALSE
  )

xgb.importance(model = xgboost_1)

cor(
  x = df_test$y,
  y = predict(xgboost_1, as.matrix(df_test[, c("a", "b")]))
)

#with multicollinearity
xgboost_2 <- xgboost(
  data = as.matrix(df_train[, c("a", "b", "c", "d")]),
  label = df_train$y,
  objective = "reg:squarederror",
  nrounds = 100,
  verbose = FALSE
)

xgb.importance(model = xgboost_2)

cor(
  x = df_test$y,
  y = predict(xgboost_2, as.matrix(df_test[, c("a", "b", "c", "d")]))
)
