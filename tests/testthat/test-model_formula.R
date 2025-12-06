testthat::test_that("`model_formula()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors_numeric)

  #null df
  testthat::expect_error(
    x <- model_formula(),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_error(
    x <- model_formula(
      df = vi_smol,
      response = NULL
    ),
    regexp = "argument 'response' cannot be NULL"
  )

  #additive formula
  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3]
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )

  #using a formula in a model
  m <- stats::lm(
    formula = x,
    data = vi_smol
  )

  testthat::expect_true(
    inherits(x = m, what = "lm")
  )

  #polynomial formula
  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    term_f = "poly",
    term_args = "degree = 3, raw = TRUE"
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )

  m <- stats::lm(
    formula = x,
    data = vi_smol
  )

  testthat::expect_true(
    inherits(x = m, what = "lm")
  )

  #gam formula
  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    term_f = "s"
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )

  m <- mgcv::gam(
    formula = x,
    data = vi_smol
  )

  testthat::expect_true(
    inherits(x = m, what = "gam")
  )

  #random effect
  testthat::expect_error(
    x <- model_formula(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric[1:3],
      random_effects = 2
    ),
    regexp = "argument 'random_effects' must be a character string or vector"
  )

  testthat::expect_error(
    x <- model_formula(
      df = vi_smol,
      response = "vi_numeric",
      predictors = c(vi_predictors_numeric[1:3], "country_name"),
      random_effects = "country_name"
    ),
    regexp = "argument 'random_effects' must name variables not in argument 'predictors'"
  )

  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    random_effects = "country_name" #from vi_smol$country_name
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )
})
