testthat::test_that("`autoconfig_arg_max_vif()` works", {

  data(
    vi_smol,
    vi_predictors_numeric
    )

  #user provided
  max_vif <- 2.5

  x <- autoconfig_arg_max_vif(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    max_cor = NULL,
    max_vif = max_vif,
    quiet = FALSE
  )

  testthat::expect_true(
    x == max_vif
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

  #wrong value
  max_vif <- -0.25

  testthat::expect_message(
    x <- autoconfig_arg_max_vif(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = max_vif,
      quiet = FALSE
    ),
    regexp = "invalid 'max_vif' value"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == 2.5
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

  #NULL value
  max_vif <- NULL

  testthat::expect_message(
    x <- autoconfig_arg_max_vif(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = max_vif,
      quiet = FALSE
    ),
    regexp = "autoconfiguring 'max_vif'"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == 2.5
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

  #cor value
  max_vif <- NULL
  max_cor <- 0.5

  testthat::expect_message(
    x <- autoconfig_arg_max_vif(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = max_cor,
      max_vif = max_vif,
      quiet = FALSE
    ),
    regexp = "autoconfiguring 'max_vif'"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == 1.85
  )

  testthat::expect_true(
    attributes(x)$valid == TRUE
  )

})
