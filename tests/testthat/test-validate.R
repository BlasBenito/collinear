testthat::test_that("`validate_df()` works", {

  data(vi)
  vi <- validate_df(df = vi)

  testthat::expect_true(attributes(vi)$validated)

})

testthat::test_that("`validate_predictors()` works", {

  data(vi, vi_predictors)

  vi <- validate_df(df = vi)

  vi_numerics <- identify_predictors_numeric(
    df = vi
  )

  #without predictors
  #without response
  #must contain only numeric columns
  predictors <- validate_predictors(
    df = vi
    )

  testthat::expect_true(
    attributes(predictors)$validated
    )

  testthat::expect_true(
    length(predictors) == ncol(vi)
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi))
  )

  #without predictors
  #with response
  #must contain all df columns but the response
  predictors <- validate_predictors(
    df = vi,
    response = "vi_numeric"
  )

  testthat::expect_true(
    !("vi_numeric" %in% predictors)
  )

  #with predictors
  #with response
  #must contain all predictors
  predictors <- validate_predictors(
    df = vi,
    response = "vi_numeric",
    predictors = vi_predictors
  )

  testthat::expect_true(
    all(predictors %in% vi_predictors)
  )

})

testthat::test_that("`validate_response()` works", {

  data(vi)

  vi <- validate_df(df = vi)

  response <- validate_response(
    df = vi,
    response = "vi_numeric"
    )

  testthat::expect_true(
    attributes(response)$validated
    )

})
