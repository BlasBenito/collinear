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

  #few rows
  testthat::expect_error(
    x <- cor_matrix(
      df = vi[1:2, ],
      predictors = predictors
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- cor_matrix(
      df = vi[1:9, ],
      predictors = predictors
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- cor_matrix(
      df = vi[1:29, ],
      predictors = predictors
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()



})
