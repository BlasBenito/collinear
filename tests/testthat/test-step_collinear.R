test_that("step_collinear works!", {
  testthat::skip_on_cran()

  data(vi_smol, vi_predictors_numeric)

  test_selection <- collinear(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  test_selection <- test_selection$vi_numeric$selection

  vi_formula <- model_formula(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric
  )

  rec <- recipes::recipe(
    formula = vi_formula,
    data = vi_smol
  ) |>
    step_collinear(
      recipes::all_predictors()
    )

  rec_prep <- recipes::prep(
    rec,
    training = vi_smol
  )

  baked <- recipes::bake(
    rec_prep,
    new_data = vi_smol
  )

  testthat::expect_true(is.data.frame(baked))

  testthat::expect_true(
    all(test_selection %in% colnames(baked))
  )

  testthat::expect_true(
    all(c("vi_numeric", rec_prep$steps[[1]]$selected) %in% names(baked))
  )
})
