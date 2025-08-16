testthat::test_that("`validate_arg_predictors_cor()` works", {

  data(vi)

  df <- vi[1:1000, ]

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_cor(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #must contain all columns
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    attributes(predictors)$validated_cor
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi))
  )


  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "hola",
      quiet = FALSE
    ),
    regexp = "no predictors available"
  ) |>
    suppressMessages()

  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "hola",
      quiet = FALSE
    ),
    regexp = "hola"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  testthat::expect_true(
    is.null(predictors)
  )

  #only one predictor
  #predictor not in df
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "vi_numeric",
      quiet = FALSE
    )
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "vi_numeric",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    length(predictors) == 1
  )

  #with constant predictors
  vi$zero_variance <- 1
  vi$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = TRUE
    )
  )

  testthat::expect_true(
    !"zero_variance" %in% predictors
  )

  testthat::expect_true(
    !"constant" %in% predictors
  )


})
