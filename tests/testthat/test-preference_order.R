testthat::test_that("`f_gam_deviance()` works", {
  data(vi)
  result <- f_gam_deviance(x = "growing_season_length", y = "vi_mean", df = vi)
  testthat::expect_true(is.numeric(result), info = "Result should be a numeric value.")
  testthat::expect_true(!is.na(result), info = "Result should not be NA.")
})

testthat::test_that("`f_rf_deviance()` works", {
  data(vi)
  result <- f_rf_deviance(x = "growing_season_length", y = "vi_mean", df = vi)
  testthat::expect_true(is.numeric(result), info = "Result should be a numeric value.")
  testthat::expect_true(!is.na(result), info = "Result should not be NA.")
})

testthat::test_that("`f_rsquared()` works", {
  data(vi)
  result <- f_rsquared(x = "growing_season_length", y = "vi_mean", df = vi)
  testthat::expect_true(is.numeric(result), info = "Result should be a numeric value.")
  testthat::expect_true(!is.na(result), info = "Result should not be NA.")
})

testthat::test_that("`preference_order()` works", {

  data(vi, vi_predictors)
  vi <- vi[1:1000, ]
  preference.order <- preference_order(
    df = vi, response = "vi_mean",
    predictors = vi_predictors, f = f_rsquared, workers = 1
  )

  testthat::expect_true(
    is.data.frame(preference.order),
    info = "Result should be a data frame."
    )

  testthat::expect_true(
    all(c("predictor", "preference") %in% colnames(preference.order)),
    info = "Result should have columns 'predictor' and 'preference'."
    )

  selected.predictors <- cor_select(
    df = vi, response = "vi_mean",
    predictors = vi_predictors, preference_order = preference.order,
    max_cor = 0.75
  )

  testthat::expect_true(
    is.character(selected.predictors),
    info = "Result should be a character vector."
    )

  testthat::expect_true(
    length(selected.predictors) > 0,
    info = "There should be selected predictors."
    )

  selected.predictors.cor <- cor_df(
    df = vi, response = "vi_mean",
    predictors = selected.predictors
  )

  testthat::expect_true(
    is.data.frame(selected.predictors.cor),
    info = "Result should be a data frame."
    )

  f_rmse <- function(x, y, df) {
    xy <- scale(na.omit(df[, c(x, y)]))
    1 - sqrt(mean((xy[, 1] - xy[, 2])^2))
  }

  preference.order <- preference_order(
    df = vi, response = "vi_mean",
    predictors = vi_predictors, f = f_rmse, workers = 1
  )

  testthat::expect_true(
    is.data.frame(preference.order),
    info = "Result should be a data frame."
    )

  testthat::expect_true(
    all(c("predictor", "preference") %in% colnames(preference.order)),
    info = "Result should have columns 'predictor' and 'preference'."
    )

})
