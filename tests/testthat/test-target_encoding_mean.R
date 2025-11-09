testthat::test_that("`target_encoding_mean()` works", {

  data(vi_smol)

  #target_encoding_mean
  df <- target_encoding_mean(
    df = vi_smol,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "soil_type_encoded_1",
    smoothing = 0
  )

  testthat::expect_true(
    class(df[["soil_type"]]) == "factor"
  )

  testthat::expect_true(
    class(df[["soil_type_encoded_1"]]) == "numeric"
  )

  df <- target_encoding_mean(
    df = df,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "soil_type_encoded_2",
    smoothing = 30
  )

  testthat::expect_true(
    class(df[["soil_type_encoded_2"]]) == "numeric"
  )

  testthat::expect_true(
    cor(df$soil_type_encoded_1, df$soil_type_encoded_2) < 1
  )

  df <- target_encoding_mean(
    df = vi_smol,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = NULL,
    smoothing = 30
  )

  testthat::expect_true(
    "soil_type__encoded" %in% colnames(df)
  )

})
