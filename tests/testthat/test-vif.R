testthat::test_that("`vif()` works", {

  # numeric types ----
  data(vi_smol, vi_predictors_numeric)

  m <- cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_no_message(
    v <- vif(m = m)
  )

  testthat::expect_true(
    is.numeric(v)
  )

  testthat::expect_true(
    length(v) == length(vi_predictors_numeric)
  )

  testthat::expect_true(
    all(names(v) %in% vi_predictors_numeric)
  )

  #no input
  testthat::expect_error(
    v <- vif(m = NULL)
  )

  #matrix with wrong dimensions
  testthat::expect_error(
    v <- vif(m = m[1:nrow(m), 2:ncol(m)])
  )

  testthat::expect_error(
    v <- vif(m = 1)
  )

  #matrix without dimnames
  dimnames(m) <- NULL
  testthat::expect_error(
    v <- vif(m = m)
  )

  #perfect correlations
  m <- matrix(
    data = rep(x = 1, times = 100),
    ncol = 10,
    nrow = 10
  )

  dimnames(m) <- list(
    letters[1:10],
    letters[1:10]
    )

  class(m) <- c("collinear_cor_matrix", class(m))

  testthat::expect_message(
    v <- vif(m = m),
    regexp = "VIF values may be numerically unstable due to severe multicollinearity"
  )

  testthat::expect_true(
    all(is.infinite(v))
  )

})
