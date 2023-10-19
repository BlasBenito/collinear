testthat::test_that("`validate_df()` works", {
  data(vi)
  vi <- validate_df(df = vi)
  testthat::expect_true(attributes(vi)$validated)
})

testthat::test_that("`validate_predictors()` works", {
  data(vi, vi_predictors)
  vi <- validate_df(df = vi)
  vi_predictors <- validate_predictors(df = vi, predictors = vi_predictors)
  testthat::expect_true(attributes(vi_predictors)$validated)
})

testthat::test_that("`validate_response()` works", {
  data(vi)
  vi <- validate_df(df = vi)
  response <- validate_response(df = vi, response = "vi_mean")
  testthat::expect_true(attributes(response)$validated)
})
