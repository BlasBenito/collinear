library(collinear)
library(spatialData)
library(future)
library(progressr)

packageVersion("collinear")
#[1] ‘3.0.1’

data(vi_smol, vi_predictors, package = "spatialData")

future::plan(
  strategy = future::multisession,
  workers = future::availableCores()
)
progressr::handlers(global = TRUE)

selection <- collinear(
  df = vi,
  responses = "vi_numeric",
  predictors = vi_predictors,
  f = f_numeric_rf
)

#shows metric = "custom"
selection$vi_numeric$preference_order

#preference_order with f_auto
preference_f_auto <- collinear::preference_order(
  df = vi,
  responses = "vi_numeric",
  predictors = vi_predictors,
  f = collinear::f_auto,
  quiet = TRUE
)

#shows metric = "R-squared"
head(preference_f_auto)

#checking with preference order
preference_f_numeric_rf <- collinear::preference_order(
  df = vi,
  responses = "vi_numeric",
  predictors = vi_predictors,
  f = collinear::f_numeric_rf,
  quiet = TRUE
)

#shows metric = "R-squared"
head(preference_f_numeric_rf)


future::plan(
  strategy = future::sequential
)
