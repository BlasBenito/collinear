testthat::test_that("`validate_arg_predictors_vif()` works", {

  data(
    vi_smol,
    vi_predictors_numeric,
    vi_predictors
    )

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_vif(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "these predictors are categorical, logical, or have near-zero variance"
  ) |>
    suppressMessages()

  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "not enough rows in `df` to process all predictors"
  ) |>
    suppressMessages()

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = NULL,
      quiet = TRUE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    attributes(predictors)$validated_vif
  )

  testthat::expect_true(
    all(predictors %in% identify_predictors_numeric(
      df = vi,
      predictors = colnames(vi)
    ))
  )

  #not enough rows
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol[1:9, ],
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "not enough rows"
  ) |>
    suppressMessages()

  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = "hola",
      quiet = TRUE
    ),
    regexp = "no valid predictors available"
  )

  testthat::expect_true(
    is.null(predictors)
  )

  #only one predictor
  #predictor not in df
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = "vi_numeric",
      quiet = FALSE
    ),
    regexp = "only one numeric predictor"
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
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
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "have near zero variance"
  ) |>
    suppressMessages()

  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi_smol,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "have constant values"
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
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
