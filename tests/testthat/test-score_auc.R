testthat::test_that("`score_auc()` works", {
  testthat::skip_on_cran()

  data(vi_smol, package = "spatialData")

  #perfect prediction
  x <- score_auc(
    o = vi_smol$vi_binomial,
    p = vi_smol$vi_numeric
  )

  testthat::expect_equal(x, 1, tolerance = 1e-10)

  #random prediction
  x <- score_auc(
    o = vi_smol$vi_binomial,
    p = runif(n = nrow(vi_smol))
  )

  testthat::expect_true(
    x < 1
  )

  #NA in input vectors
  o <- vi_smol$vi_binomial
  p <- vi_smol$vi_numeric
  o[1:5] <- NA
  p[10:15] <- NA

  x <- score_auc(
    o = o,
    p = p
  )

  #error
  testthat::expect_error(
    x <- score_auc(
      o = runif(n = nrow(vi_smol)),
      p = vi_smol$vi_binomial
    )
  )

  testthat::expect_error(
    x <- score_auc(
      o = NULL,
      p = vi_smol$vi_binomial
    )
  )

  testthat::expect_error(
    x <- score_auc(
      o = vi_smol$vi_binomial,
      p = NULL
    )
  )

  # NA handling (issue #19)
  o_with_na <- vi_smol$vi_binomial
  o_with_na[1] <- NA

  x <- score_auc(
    o = o_with_na,
    p = vi_smol$vi_numeric
  )
  testthat::expect_true(is.numeric(x))

  p_with_na <- vi_smol$vi_numeric
  p_with_na[1] <- NA

  x <- score_auc(
    o = vi_smol$vi_binomial,
    p = p_with_na
  )
  testthat::expect_true(is.numeric(x))

  o_with_na[2] <- NA
  p_with_na[2] <- NA

  x <- score_auc(
    o = o_with_na,
    p = p_with_na
  )
  testthat::expect_true(is.numeric(x))
})
