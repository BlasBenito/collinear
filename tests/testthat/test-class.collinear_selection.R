testthat::test_that("`class.collinear_selection()` works", {

  data(
    vi_smol,
    vi_predictors_categorical,
    vi_predictors_numeric,
    vi_predictors
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
    selection = x,
    quiet = FALSE
  )

  testthat::expect_true(
    "collinear_selection" %in% class(y)
  )

  testthat::expect_true(
    all(c("response", "selection", "formulas", "df", "preference_order") %in% names(y))
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(y$formulas))
  )

  testthat::expect_true(
    all(x == y$selection)
  )

  #NULL response
  y <- class.collinear_selection(
    df = vi_smol,
    response = NULL,
    preference_order = preference_df,
    selection = x,
    quiet = FALSE
  )

  testthat::expect_null(
    y$formulas
  )

  #vector preference order
  y <- class.collinear_selection(
    df = vi_smol,
    response = NULL,
    preference_order = x,
    selection = x,
    quiet = FALSE
  )

  testthat::expect_true(
    all(y$preference_order %in% x)
  )


})
