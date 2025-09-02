testthat::test_that("`validate_arg_predictors_cor()` works", {

  data(
    vi_smol,
    vi_predictors_numeric,
    vi_predictors
    )

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_cor(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi_smol,
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
    all(predictors %in% colnames(vi_smol))
  )


  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "hola",
      quiet = FALSE
    ),
    regexp = "no valid predictors available"
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
      df = vi_smol,
      predictors = "vi_numeric",
      quiet = FALSE
    ),
    regexp = "only one predictor in argument 'predictors'"
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi_smol,
      predictors = "vi_numeric",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    length(predictors) == 1
  )

  #with constant predictors
  vi_smol$zero_variance <- 1
  vi_smol$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi_smol,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "these predictors have near zero variance and will be ignored"
  ) |>
    suppressMessages()

  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi_smol,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "these predictors have constant values and will be ignored"
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi_smol,
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
