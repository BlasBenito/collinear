testthat::test_that("`vif_df()` works", {

  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  #create a few perfect correlations in vi
  #reduce correlation in predictors with cor_select()
  vi_predictors <- cor_select(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors,
    max_cor = 0.75
  )

  # Test with only numeric predictors
  df <- vif_df(
    df = vi,
    predictors = vi_predictors
    )

  # Check that the result is a data frame
  testthat::expect_true(is.data.frame(df))

  # Check that the data frame has the expected structure
  testthat::expect_equal(
    ncol(df), 2,
    info = "Expected two columns"
    )

  testthat::expect_true(
    all(c("variable", "vif") %in% colnames(df)),
    info = "Expected 'variable' and 'vif' columns"
    )

  # Check that all VIF values are numeric
  testthat::expect_true(
    all(is.numeric(df$vif)),
    info = "Expected all VIF values to be numeric"
    )

  # Test with response variable included
  df <- vif_df(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors
    )

  # Check that the result is a data frame
  testthat::expect_true(is.data.frame(df))

  # Check that the data frame has the expected structure
  testthat::expect_equal(
    ncol(df), 2,
    info = "Expected two columns"
  )

  testthat::expect_true(
    all(c("variable", "vif") %in% colnames(df)),
    info = "Expected 'variable' and 'vif' columns"
  )

  # Check that all VIF values are numeric
  testthat::expect_true(
    all(is.numeric(df$vif)),
    info = "Expected all VIF values to be numeric"
  )

})
