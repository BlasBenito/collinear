testthat::test_that("`target_encoding_methods()` works", {

  data(vi)

  #target_encoding_mean
  df <- target_encoding_mean(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type",
    smoothing = 0
  )

  #TODO: add tests here

  df <- target_encoding_mean(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type",
    smoothing = 30
  )

  #TODO: add tests here

  #target_encoding_rank
  df <- target_encoding_rank(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type"
  )

  #TODO: add tests here


  #target_encoding_loo
  df <- target_encoding_loo(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type"
  )

  #TODO: add tests here


})
