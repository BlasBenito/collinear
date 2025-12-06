testthat::test_that("`score_r2()` works", {
  testthat::skip_on_cran()

  data(vi_smol)

  #perfect prediction
  x <- score_cramer(
    o = vi_smol$vi_categorical,
    p = vi_smol$vi_categorical
  )

  testthat::expect_true(
    x == 1
  )

  #random prediction
  x <- score_cramer(
    o = vi_smol$vi_categorical,
    p = vi_smol$koppen_zone
  )

  testthat::expect_true(
    x < 1
  )

  #error
  testthat::expect_error(
    x <- score_cramer(
      o = NULL,
      p = vi_smol$vi_categorical
    )
  )

  testthat::expect_error(
    x <- score_cramer(
      o = vi_smol$vi_categorical,
      p = NULL
    )
  )
})
