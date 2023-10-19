testthat::test_that("`cor_df()` works", {

  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  #without response
  df <- cor_df(
    df = vi,
    predictors = vi_predictors
    )

  testthat::expect_true(
    is.data.frame(df),
    info = "Result should be a data frame."
  )

  testthat::expect_true(
    all(names(df) %in% c("x", "y", "correlation")),
    info = "Data frame should have columns 'x', 'y', and 'correlation'."
  )

  testthat::expect_true(
    nrow(df) > 0,
    info = "There should be correlations calculated."
  )

  #with response
  df <- cor_df(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors
    )

  testthat::expect_true(
    is.data.frame(df),
    info = "Result should be a data frame."
  )

  testthat::expect_true(
    all(names(df) %in% c("x", "y", "correlation")),
    info = "Data frame should have columns 'x', 'y', and 'correlation'."
  )

  testthat::expect_true(
    nrow(df) > 0,
    info = "There should be correlations calculated."
  )

})
