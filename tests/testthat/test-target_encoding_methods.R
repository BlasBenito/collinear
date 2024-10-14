testthat::test_that("`target_encoding_methods()` works", {

  df <- vi[1:1000, ]

  #target_encoding_mean
  df <- target_encoding_mean(
    df = df,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "new_column_1",
    smoothing = 0
  )

  testthat::expect_true(
    class(vi[["soil_type"]]) == "factor"
  )

  testthat::expect_true(
    class(df[["new_column_1"]]) == "numeric"
  )

  df <- target_encoding_mean(
    df = df,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "new_column_2",
    smoothing = 30
  )

  testthat::expect_true(
    class(df[["new_column_2"]]) == "numeric"
  )

  testthat::expect_true(
    cor(df$new_column_1, df$new_column_2) < 1
  )


  #rank
  df <- target_encoding_rank(
    df = df,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "new_column_3",
    smoothing = 0
  )

  testthat::expect_true(
    class(df[["new_column_3"]]) == "integer"
  )

  #loo
  df <- target_encoding_loo(
    df = df,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "new_column_4",
    smoothing = 0
  )

  testthat::expect_true(
    class(df[["new_column_4"]]) == "numeric"
  )

  testthat::expect_true(
    cor(df$new_column_3, df$new_column_4) < 1
  )


})
