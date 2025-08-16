testthat::test_that("`validate_arg_predictors_vif()` works", {

  data(vi)

  df <- vi[1:1000, ]

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_vif(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #must contain only numeric columns
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "are not numeric and will be ignored"
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
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
      df = vi[1:9, ],
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "not enough rows"
  )

  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = "hola",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    is.null(predictors)
  )

  #only one predictor
  #predictor not in df
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = "vi_numeric",
      quiet = FALSE
    ),
    regexp = "only one numeric predictor"
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
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
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "have near zero variance"
  ) |>
    suppressMessages()

  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "have constant values"
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
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
