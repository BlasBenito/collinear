testthat::test_that("`summary.collinear_selection()` works", {

  data(
    vi_smol,
    vi_predictors_numeric
    )

  #normal usage
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  x <- collinear_select(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    preference_order = preference_df,
    quiet = TRUE
  )

  y <- class.collinear_selection(
    df = vi_smol,
    response = "vi_numeric",
    preference_order = preference_df,
    selection = x[1],
    quiet = FALSE
  )

  testthat::expect_output(
    z <- summary(y),
    regexp = "response"
  )

  y <- class.collinear_selection(
    df = vi_smol,
    response = "vi_numeric",
    preference_order = preference_df,
    selection = x,
    quiet = FALSE
  )

  testthat::expect_output(
    z <- summary(y),
    regexp = "response"
  )


  testthat::expect_true(
    all(y$selection %in% z)
  )

  y <- class.collinear_selection(
    df = vi_smol,
    response = NULL,
    preference_order = preference_df,
    selection = x,
    quiet = FALSE
  )

  testthat::expect_output(
    z <- summary(y),
    regexp = "selection"
  )

  testthat::expect_true(
    all(y$selection %in% z)
  )


  y <- class.collinear_selection(
    df = vi_smol,
    response = NULL,
    preference_order = preference_df,
    selection = NULL,
    quiet = FALSE
  )

  z <- summary(y)

  testthat::expect_null(
    z$selection
  )

})
