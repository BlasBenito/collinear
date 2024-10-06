testthat::test_that("`cor_matrix()` works", {

  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  #without response, input from cor_df()
  predictors <- vi_predictors[1:15]

  df <- cor_df(
    df = vi,
    predictors = predictors
    )

  m <- cor_matrix(df = df)

  testthat::expect_true(
    is.matrix(m),
    info = "Result should be a matrix."
  )

  testthat::expect_true(
    all(rownames(m) == colnames(m)),
    info = "Row and column names should be the same."
  )

  testthat::expect_true(
    all(rownames(m) %in% predictors),
    info = "Row and column names should match the variables."
  )

  testthat::expect_true(
    all(diag(m) == 1),
    info = "Diagonal elements should be 1."
  )



})
