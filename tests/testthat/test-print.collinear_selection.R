testthat::test_that("`summary.collinear_selection()` works", {
  testthat::skip_on_cran()

  data(
    vi_smol,
    vi_predictors_numeric
  )

  x <- collinear(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    preference_order = NULL,
    quiet = TRUE
  )

  testthat::expect_output(
    print(x$vi_numeric),
    regexp = "vi_numeric"
  )

  testthat::expect_output(
    print(x$vi_numeric),
    regexp = "selection"
  )

  testthat::expect_output(
    print(x$vi_numeric),
    regexp = "formulas"
  )

  testthat::expect_output(
    print(x$vi_numeric),
    regexp = "df"
  )

  testthat::expect_output(
    print(x$vi_numeric),
    regexp = "preference order"
  )

  x <- collinear(
    df = vi_smol,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
    preference_order = NULL,
    quiet = TRUE
  )

  testthat::expect_output(
    print(x$vi_categorical),
    regexp = "formula"
  )

  #number of terms
  x$vi_categorical$selection <- x$vi_categorical$selection[1:6]

  testthat::expect_output(
    print(x$vi_categorical),
    regexp = "formula"
  )
})
