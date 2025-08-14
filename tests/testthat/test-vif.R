testthat::test_that("`vif()` works", {

  # numeric types ----
  predictors <- vi_predictors_numeric[1:10]
  df <- vi[1:1000, ]

  m <- cor_matrix(
    df = df,
    predictors = predictors
  )

  testthat::expect_no_message(
    v <- vif(m = m)
  )

  testthat::expect_true(
    is.numeric(v)
  )

  testthat::expect_true(
    length(v) == length(predictors)
  )

  testthat::expect_true(
    all(names(v) %in% predictors)
  )

  #no input
  testthat::expect_error(
    v <- vif(m = NULL)
  )

  #matrix with wrong dimensions
  testthat::expect_error(
    v <- vif(m = m[1:nrow(m), 2:ncol(m)])
  )

  #matrix without dimnames
  dimnames(m) <- NULL
  testthat::expect_error(
    v <- vif(m = m)
  )


})
