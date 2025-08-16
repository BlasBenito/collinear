testthat::test_that("`validate_arg_predictors()` works", {

  data(vi)

  df <- vi[1:1000, ]

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors(
      df = NULL,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #without response
  #must contain only numeric columns
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = df,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    length(predictors) == ncol(df)
  )

  testthat::expect_true(
    all(predictors %in% colnames(df))
  )

  #without predictors
  #with response
  #must contain all df columns but the response
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = df,
      response = "vi_numeric",
      quiet = FALSE
    )
  )

  testthat::expect_true(
    !("vi_numeric" %in% predictors)
  )

  #with predictors
  #with response
  #must contain all predictors
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = df,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    all(predictors %in% vi_predictors)
  )

  #with constant predictors
  df$zero_variance <- 1
  df$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors(
      df = df,
      response = "vi_numeric",
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    ),
    regexp = "have constant values"
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = df,
      response = "vi_numeric",
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
