testthat::test_that("`target_encoding_loo()` works", {

  data(vi_smol)

  #rank
  df <- target_encoding_loo(
    df = vi_smol,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "encoded_column"
  )

  testthat::expect_true(
    class(df[["encoded_column"]]) == "numeric"
  )


  df <- target_encoding_loo(
    df = vi_smol,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = NULL
  )

  testthat::expect_true(
    "soil_type__encoded" %in% colnames(df)
  )

})
