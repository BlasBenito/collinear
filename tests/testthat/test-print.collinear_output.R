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
    print(x),
    regexp = "vi_numeric"
  )

  x <- collinear(
    df = vi_smol,
    response = c("vi_numeric", "vi_categorical"),
    predictors = vi_predictors_numeric,
    preference_order = NULL,
    quiet = TRUE
  )

  testthat::expect_output(
    print(x),
    regexp = "vi_numeric"
  )

  testthat::expect_output(
    print(x),
    regexp = "vi_categorical"
  )

  testthat::expect_output(
    print(x),
    regexp = "selection"
  )

  testthat::expect_output(
    print(x),
    regexp = "formulas"
  )

  testthat::expect_output(
    print(x),
    regexp = "df"
  )

  testthat::expect_output(
    print(x),
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
