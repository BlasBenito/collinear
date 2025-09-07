testthat::test_that("`vif_df()` works", {

  data(vi, vi_predictors_numeric)

  # numeric types ----
  predictors <- vi_predictors_numeric[1:10]
  df <- vi[1:1000, ]

  testthat::expect_no_message(
    vif.df <- vif_df(
      df = df,
      predictors = predictors
    )
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
  )

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
    ),
    regexp = "no numeric columns in argument 'predictors'"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.data.frame(vif.df)
  )

  testthat::expect_true(
    nrow(vif.df) == 0
  )

  # edge cases ----

  #no arguments
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
    x <- vif_df(
      df = df[1:2, ],
      predictors = predictors
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- vif_df(
      df = df[1:9, ],
      predictors = predictors
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- vif_df(
      df = df[1:29, ],
      predictors = predictors
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()


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
    ),
    regexp = "only one numeric predictor"
  )


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
