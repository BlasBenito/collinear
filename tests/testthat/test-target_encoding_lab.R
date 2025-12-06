testthat::test_that("`target_encoding_lab()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors, vi_predictors_numeric, vi_predictors_categorical)

  #stops

  #null response
  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = NULL
    ),
    regexp = "argument 'response' is NULL, skipping target encoding"
  )

  #categorical response
  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_categorical"
    ),
    regexp = "'response' column 'vi_categorical' is not numeric, skipping target encoding"
  ) |>
    suppressMessages()

  #no categorical predictors
  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric
    ),
    regexp = "no categorical predictors in argument 'predictors', skipping target encoding"
  ) |>
    suppressMessages()

  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_categorical,
      overwrite = "hola"
    ),
    regexp = "argument 'overwrite' must be logical, resetting it to 'FALSE'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_categorical,
      overwrite = FALSE,
      encoding_method = NULL
    ),
    regexp = "argument 'encoding_method' is NULL, skipping target encoding"
  ) |>
    suppressMessages()

  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_categorical,
      overwrite = FALSE,
      encoding_method = "hola"
    ),
    regexp = "argument 'encoding_method' is not valid, resetting it to 'loo'"
  ) |>
    suppressMessages()

  #smoothing reset to zero
  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_categorical,
      overwrite = FALSE,
      encoding_method = "mean",
      smoothing = nrow(vi_smol) + 1
    ),
    regexp = "invalid values in argument 'smoothing', resetting it to '0'"
  ) |>
    suppressMessages()

  #several smoothing and overwrite = TRUE
  testthat::expect_message(
    df <- target_encoding_lab(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_categorical,
      overwrite = TRUE,
      encoding_method = "mean",
      smoothing = c(10, 20),
    ),
    regexp = "only one 'smoothing' value allowed when 'overwrite = TRUE', resetting it to '10'"
  ) |>
    suppressMessages()

  #proper call
  df <- target_encoding_lab(
    df = vi_smol,
    response = "vi_numeric",
    predictors = "koppen_zone",
    overwrite = FALSE,
    encoding_method = c("mean", "rank", "loo"),
    smoothing = c(0, 30),
    quiet = TRUE
  )

  # Check if the result is a data frame
  testthat::expect_true(
    is.data.frame(df)
  )

  # Check if the encoded variables have been added
  testthat::expect_true(
    "koppen_zone__encoded_mean" %in% colnames(df)
  )

  testthat::expect_true(
    "koppen_zone__encoded_mean__smoothing_30" %in% colnames(df)
  )

  testthat::expect_true(
    "koppen_zone__encoded_rank" %in% colnames(df)
  )

  testthat::expect_true(
    "koppen_zone__encoded_loo" %in% colnames(df)
  )

  # Check if encoding methods have been applied
  testthat::expect_true(
    is.numeric(df$koppen_zone__encoded_mean)
  )

  testthat::expect_true(
    is.numeric(df$koppen_zone__encoded_mean__smoothing_30)
  )

  testthat::expect_true(
    is.numeric(df$koppen_zone__encoded_rank)
  )

  testthat::expect_true(
    is.numeric(df$koppen_zone__encoded_loo)
  )
})
