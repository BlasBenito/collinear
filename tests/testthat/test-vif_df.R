testthat::test_that("`vif_df()` works", {

  # numeric types ----
  predictors <- vi_predictors_numeric[1:10]
  df <- vi[1:1000, ]

  vif.df <- vif_df(
    df = df,
    predictors = predictors
  )

  testthat::expect_true(
    is.data.frame(vif.df)
  )

  testthat::expect_true(
    all(names(vif.df) %in% c("predictor", "vif"))
  )

  testthat::expect_true(
    nrow(vif.df) == length(predictors)
  )

  # mixed types ----
  predictors <- vi_predictors[1:10]
  df <- vi[1:1000, ]

  testthat::expect_message(
    vif.df <- vif_df(
      df = df,
      predictors = predictors
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(vif.df)
  )

  testthat::expect_true(
    all(names(vif.df) %in% c("predictor", "vif"))
  )

  testthat::expect_true(
    length(vif.df$predictor) < length(predictors)
  )

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  testthat::expect_message(
    vif.df <- vif_df(
      df = df,
      predictors = predictors
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(vif.df)
  )

  testthat::expect_true(
    all(is.na(vif.df[1, ]))
  )

  # edge cases ----

  #no df
  testthat::expect_error(
    vif.df <- vif_df(
      df = NULL,
      predictors = NULL
    )
  )

  #predictors only
  testthat::expect_error(
    vif.df <- vif_df(
      df = NULL,
      predictors = vi_predictors
    )
  )

  #few rows
  testthat::expect_error(
    vif.df <- vif_df(
      df = vi[1, ],
      predictors = vi_predictors
    )
  )


  #no predictors
  predictors <- vi_predictors_numeric[1:5]

  vif.df <- vif_df(
    df = df[, predictors, drop = FALSE],
    predictors = NULL
  )

  testthat::expect_true(
    all(
      vif.df$predictor %in% colnames(df)
    )
  )

  #single predictor
  predictors <- vi_predictors_numeric[1]

  testthat::expect_message(
    vif.df <- vif_df(
      df = df,
      predictors = predictors
    )
  ) |>
    suppressMessages()


  testthat::expect_true(
    is.data.frame(vif.df)
  )

  testthat::expect_true(
    nrow(vif.df) == 1
  )

  testthat::expect_true(
    vif.df$vif == 0
  )

})
