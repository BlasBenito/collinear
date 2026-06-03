testthat::test_that("`model_formula()` works", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors, package = "spatialData")
  vi_predictors_numeric <- identify_numeric_variables(
    df = vi_smol,
    predictors = vi_predictors
  )$valid

  #null df
  testthat::expect_error(
    x <- model_formula(),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_error(
    x <- model_formula(
      df = vi_smol,
      response = NULL,
      quiet = TRUE
    ),
    regexp = "argument 'response' cannot be NULL"
  )

  #additive formula
  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    quiet = TRUE
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
    term_args = "degree = 3, raw = TRUE",
    quiet = TRUE
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
    term_f = "s",
    quiet = TRUE
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
      random_effects = 2,
      quiet = TRUE
    ),
    regexp = "argument 'random_effects' must be a character string or vector"
  )

  testthat::expect_error(
    x <- model_formula(
      df = vi_smol,
      response = "vi_numeric",
      predictors = c(vi_predictors_numeric[1:3], "country_name"),
      random_effects = "country_name",
      quiet = TRUE
    ),
    regexp = "argument 'random_effects' must name variables not in argument 'predictors'"
  )

  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    random_effects = "country_name", #from vi_smol$country_name
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )

  # sf geometry column handling (issue #17)
  sf_df <- vi_smol[1:50, c("vi_numeric", vi_predictors_numeric[1:3])]
  sf_df$geometry <- I(rep(list(c(0, 0)), nrow(sf_df)))
  attr(sf_df, "sf_column") <- "geometry"

  x <- model_formula(
    df = sf_df,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "formula")
  )

  # formula environment must be tiny (no captured call-frame data)
  x <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric[1:3],
    quiet = TRUE
  )
  testthat::expect_lt(as.numeric(object.size(environment(x))), 50000)
})
