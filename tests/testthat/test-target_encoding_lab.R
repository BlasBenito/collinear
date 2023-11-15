testthat::test_that("`target_encoding_lab()` works", {
  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  # Call the target_encoding_lab function
  encoded_df <- target_encoding_lab(
    df = vi,
    response = "vi_mean",
    predictors = "koppen_zone",
    encoding_methods = c("mean", "rank", "rnorm", "loo"),
    smoothing = c(0, 30),
    rnorm_sd_multiplier = c(0, 0.1, 0.2),
    white_noise = c(0, 0.1, 0.2),
    verbose = FALSE
  )

  # Check if the result is a data frame
  testthat::expect_true(is.data.frame(encoded_df), info = "Result should be a data frame.")

  # Check if the encoded variables have been added
  testthat::expect_true(
    "koppen_zone__encoded_mean" %in% colnames(encoded_df),
    info = "Encoded variable 'koppen_zone__encoded_mean' should exist."
    )

  testthat::expect_true(
    "koppen_zone__encoded_rank" %in% colnames(encoded_df),
    info = "Encoded variable 'koppen_zone__encoded_rank' should exist."
    )

  testthat::expect_true(
    "koppen_zone__encoded_rnorm" %in% colnames(encoded_df),
    info = "Encoded variable 'koppen_zone__encoded_rnorm' should exist."
    )

  testthat::expect_true(
    "koppen_zone__encoded_loo" %in% colnames(encoded_df),
    info = "Encoded variable 'koppen_zone__encoded_loo' should exist."
    )

  # Check if encoding methods have been applied
  testthat::expect_true(
    is.numeric(encoded_df$koppen_zone__encoded_mean)
    )

  testthat::expect_true(
    is.numeric(encoded_df$koppen_zone__encoded_rank)
    )

  testthat::expect_true(
    is.numeric(encoded_df$koppen_zone__encoded_rnorm)
    )

  testthat::expect_true(
    is.numeric(encoded_df$koppen_zone__encoded_loo)
    )

})
