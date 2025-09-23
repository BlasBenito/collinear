testthat::test_that("`model_formula()` works", {

  data(vi_smol, vi_predictors_numeric)

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
