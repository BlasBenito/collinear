testthat::test_that("`cor_matrix()` works", {

  data(vi_smol, vi_predictors)

  #nput from cor_df()

  df <- cor_df(
    df = vi_smol,
    predictors = vi_predictors[1:15]
    )

  m <- cor_matrix(df = df)

  testthat::expect_true(
    is.matrix(m)
  )

  testthat::expect_true(
    all(rownames(m) == colnames(m))
  )

  testthat::expect_true(
    all(rownames(m) %in% vi_predictors[1:15])
  )

  testthat::expect_true(
    all(diag(m) == 1)
  )

  #few rows
  testthat::expect_error(
    x <- cor_matrix(
      df = vi_smol[1:2, ],
      predictors = vi_predictors[1:15]
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- cor_matrix(
      df = vi_smol[1:9, ],
      predictors = vi_predictors[1:15]
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- cor_matrix(
      df = vi_smol[1:29, ],
      predictors = vi_predictors[1:15]
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()



})
