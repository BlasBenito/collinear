testthat::test_that("`f_functions()` works", {
  testthat::skip_on_cran()

  df <- f_functions()

  testthat::expect_true(
    is.data.frame(df)
  )

  testthat::expect_true(
    all(df[["name"]] %in% ls(getNamespace("collinear"), all.names = TRUE))
  )
})
