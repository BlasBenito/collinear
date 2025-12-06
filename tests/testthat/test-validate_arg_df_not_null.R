testthat::test_that("`validate_arg_df_not_null()` works", {
  testthat::skip_on_cran()

  data(vi_smol)

  testthat::expect_error(
    x <- validate_arg_df_not_null(
      df = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  x <- validate_arg_df_not_null(
    df = vi_smol
  )

  testthat::expect_true(
    all.equal(vi_smol, x)
  )

  df <- vi_smol

  attr(x = df, which = "validated") <- TRUE

  x <- validate_arg_df_not_null(
    df = df
  )
})
