testthat::test_that("`cor_df()` works", {

  data(vi, vi_predictors, vi_predictors_categorical)

  # mixed types ----
  predictors <- vi_predictors[1:10]
  df <- vi[1:1000, ]

  cor.df <- cor_df(
    df = df,
    predictors = predictors
  )

  testthat::expect_true(
    is.data.frame(cor.df)
  )

  testthat::expect_true(
    all(names(cor.df) %in% c("x", "y", "correlation"))
  )

  testthat::expect_true(
    nrow(cor.df) > 0
  )

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  cor.df <- cor_df(
    df = df,
    predictors = predictors
  )

  testthat::expect_true(
    is.data.frame(cor.df)
  )

  testthat::expect_true(
    all(names(cor.df) %in% c("x", "y", "correlation"))
  )

  testthat::expect_true(
    nrow(cor.df) > 0
  )

  # edge cases ----

  #no df
  testthat::expect_error(
    cor.df <- cor_df(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #predictors only
  testthat::expect_error(
    cor.df <- cor_df(
      df = NULL,
      predictors = vi_predictors
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #few rows
  testthat::expect_error(
    x <- cor_df(
      df = vi[1:2, ],
      predictors = predictors
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- cor_df(
      df = vi[1:9, ],
      predictors = predictors
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- cor_df(
      df = vi[1:29, ],
      predictors = predictors
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()


  #no predictors
  cor.df <- cor_df(
    df = vi[1:1000, 1:5],
    predictors = NULL
  )

  testthat::expect_true(
    all(
      unique(
        c(
          cor.df$x,
          cor.df$y
        )
      ) %in% colnames(vi)[1:5]
    )
  )

  #single predictor
  predictors <- vi_predictors[1]

  testthat::expect_message(
    cor.df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = FALSE
    ),
    regexp = "only one predictor"
  )


  testthat::expect_true(
    is.data.frame(cor.df)
  )

  testthat::expect_true(
    all(names(cor.df) %in% c("x", "y", "correlation"))
  )

  testthat::expect_true(
    nrow(cor.df) == 1
  )

  testthat::expect_true(
    cor.df$correlation == 1
  )


})
