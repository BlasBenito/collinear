testthat::test_that("`target_encoding_methods()` works", {

  data(vi)

  #target_encoding_mean
  testthat::expect_true(
    is.character(vi$soil_type)
  )

  df <- target_encoding_mean(
    df = vi,
    response = "vi_mean",
    predictor = "soil_type",
    white_noise = 0.1,
    replace = FALSE,
    verbose = FALSE
  )

  testthat::expect_true(
    "soil_type__encoded_mean__white_noise_0.1" %in% names(df)
  )

  testthat::expect_true(
    is.numeric(df$soil_type__encoded_mean__white_noise_0.1)
  )

  #target_encoding_rank
  df <- target_encoding_rank(
    df = vi,
    response = "vi_mean",
    predictor = "soil_type",
    replace = FALSE,
    verbose = FALSE
  )

  testthat::expect_true(
    "soil_type__encoded_rank" %in% names(df)
  )

  testthat::expect_true(
    is.numeric(df$soil_type__encoded_rank)
  )


  #target_encoding_loo
  df <- target_encoding_loo(
    df = vi,
    response = "vi_mean",
    predictor = "soil_type",
    white_noise = 0.1,
    replace = FALSE,
    verbose = FALSE
  )

  testthat::expect_true(
    "soil_type__encoded_loo__white_noise_0.1" %in% names(df)
  )

  testthat::expect_true(
    is.numeric(df$soil_type__encoded_loo__white_noise_0.1)
  )

  #target_encoding_rnorm
  df <- target_encoding_rnorm(
    df = vi,
    response = "vi_mean",
    predictor = "soil_type",
    rnorm_sd_multiplier = 0.1,
    replace = FALSE,
    verbose = FALSE
  )

  testthat::expect_true(
    "soil_type__encoded_rnorm__sd_multiplier_0.1" %in% names(df)
  )

  testthat::expect_true(
    is.numeric(df$soil_type__encoded_rnorm__sd_multiplier_0.1)
  )


})
