testthat::test_that("`cor_df()` works", {

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
    )
  )

  #predictors only
  testthat::expect_error(
    cor.df <- cor_df(
      df = NULL,
      predictors = vi_predictors
    )
  )

  #few rows
  testthat::expect_error(
    cor.df <- cor_df(
      df = vi[1, ],
      predictors = vi_predictors
    )
  )


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
      predictors = predictors
    )
  ) |>
    suppressMessages()

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
