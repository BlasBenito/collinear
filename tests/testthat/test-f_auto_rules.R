testthat::test_that("`f_auto_rules()` works", {


  df <- f_auto_rules()

  testthat::expect_true(
    is.data.frame(df)
  )

  testthat::expect_true(
    all(df$name %in% ls(getNamespace("collinear"), all.names=TRUE))
  )


})
