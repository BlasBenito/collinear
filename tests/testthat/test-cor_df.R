testthat::test_that("`cor_df()` works", {

  data(vi_smol, vi_predictors, vi_predictors_categorical)

  #mixed types
  x <- cor_df(
    df = vi_smol,
    predictors = vi_predictors[1:10],
    quiet = TRUE
  )

  testthat::expect_true(
    "collinear_cor_df" %in% class(x)
  )

  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) > 0
  )

  testthat::expect_true(
    nrow(
      t(
        combn(
          x = vi_predictors[1:10],
          m = 2
        )
      )
    ) == nrow(x)
  )

  testthat::expect_true(
    all(names(x) %in% c("x", "y", "correlation", "metric"))
  )

  testthat::expect_true(
    all(c("Pearson", "Cramer's V") %in% x$metric)
  )

  testthat::expect_true(
    all(x$correlation < 1)
  )



  # edge cases ----

  #no df
  testthat::expect_error(
    x <- cor_df(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #few rows
  testthat::expect_error(
    x <- cor_df(
      df = vi_smol[1, ],
      predictors = vi_predictors,
      quiet = TRUE
    ),
    regexp = "argument 'df' has fewer than 3 rows"
  )


  #no predictors
  x <- cor_df(
    df = vi_smol[, 1:5],
    predictors = NULL,
    quiet = TRUE
  )


  testthat::expect_true(
    "collinear_cor_df" %in% class(x)
  )

  testthat::expect_true(
    all(
      unique(
        c(
          x$x,
          x$y
        )
      ) %in% colnames(vi)[1:5]
    )
  )

  #single predictor
  testthat::expect_message(
    x <- cor_df(
      df = vi_smol,
      predictors = vi_predictors[1],
      quiet = FALSE
    ),
    regexp = "only one valid predictor, returning one-row dataframe"
  ) |>
    suppressMessages()


  testthat::expect_true(
    "collinear_cor_df" %in% class(x)
  )


  testthat::expect_true(
    is.data.frame(x)
  )

  testthat::expect_true(
    nrow(x) == 1
  )

  testthat::expect_true(
    all(names(x) %in% c("x", "y", "correlation", "metric"))
  )

  testthat::expect_true(x$x == vi_predictors[1])

  testthat::expect_true(x$y == vi_predictors[1])

  testthat::expect_true(x$correlation == 1)



})
