test_that("step_collinear works conditionally", {

  testthat::skip_if_not_installed("recipes")

  data(vi_smol, vi_predictors_numeric)

  rec <- recipes::recipe(
    vi_numeric ~ .,
    data = vi_smol
    ) |>
    step_collinear(
      recipes::all_predictors(),
      options = list(
        responses = "vi_numeric",
        predictors = vi_predictors_numeric
        )
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
    all(c("vi_numeric", rec_prep$steps[[1]]$selected) %in% names(baked))
    )

})
