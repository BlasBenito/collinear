testthat::test_that("`vif_df()` works", {

  data(vi_smol, vi_predictors)

  testthat::expect_no_message(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1:10]
    )
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    all(names(x) %in% c("predictor", "vif"))
  )

  testthat::expect_true(
    nrow(x) == length(vi_predictors[1:10])
  )



  # edge cases ----

  #no arguments
  testthat::expect_error(
    x <- vif_df(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )


  #single predictor
  testthat::expect_message(
    x <- vif_df(
      df = vi_smol,
      predictors = vi_predictors[1]
    ),
    regexp = "only one valid predictor"
  )


  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == 1
  )

  testthat::expect_true(
    x$vif == 0
  )

})
