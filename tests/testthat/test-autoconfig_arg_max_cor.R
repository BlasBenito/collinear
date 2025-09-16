testthat::test_that("`autoconfig_arg_max_cor()` works", {

  data(
    vi_smol,
    vi_predictors_numeric
    )

  #user provided
  max_cor <- 0.75

  x <- autoconfig_arg_max_cor(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    max_cor = max_cor,
    quiet = FALSE
  )

  testthat::expect_true(
    x == max_cor
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

  #wrong value
  max_cor <- -0.25

  testthat::expect_message(
    x <- autoconfig_arg_max_cor(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = max_cor,
      quiet = FALSE
    ),
    regexp = "invalid 'max_cor' value"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == 0.58
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

  #NULL value
  max_cor <- NULL

  testthat::expect_message(
    x <- autoconfig_arg_max_cor(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = max_cor,
      quiet = FALSE
    ),
    regexp = "autoconfiguring 'max_cor'"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == 0.58
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )


})
