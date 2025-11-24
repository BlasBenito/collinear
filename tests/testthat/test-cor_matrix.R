testthat::test_that("`cor_matrix()` works", {

  data(vi_smol, vi_predictors)

  #nput from cor_df()

  testthat::expect_warning(
    df <- cor_df(
      df = vi_smol,
      predictors = vi_predictors[1:15],
      quiet = TRUE
    ),
    regexp = "may bias the multicollinearity analysis"
  )


  m <- cor_matrix(df = df)

  testthat::expect_true(
    "collinear_cor_matrix" %in% class(m)
  )

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

  #univariate case
  testthat::expect_message(
    df <- cor_df(
      df = vi_smol,
      predictors = vi_predictors[1]
    ),
    regexp = "only one valid predictor"
  ) |>
    suppressMessages()


  m <- cor_matrix(
    df = df,
    quiet = TRUE
    )

  testthat::expect_true(
    "collinear_cor_matrix" %in% class(m)
  )

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

  testthat::expect_message(
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

  #check dots argument
  m1 <- cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_numeric[1:10],
    quiet = TRUE
  )

  attr(x = m1, which = "my_matrix") <- TRUE

  m2 <- cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_numeric[1:10],
    quiet = TRUE,
    m = m1
  )

  testthat::expect_true(
    attributes(m2)$my_matrix
  )

  testthat::expect_equal(
    object = m1, expected = m2
  )

  m3 <-  cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_numeric[11:20],
    quiet = TRUE,
    m = m1
  )

  testthat::expect_null(
    attributes(m3)$my_matrix
  )

  testthat::expect_false(
    all(colnames(m1) %in% colnames(m3))
  )


})
