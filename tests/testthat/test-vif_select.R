testthat::test_that("`vif_select()` works", {

  predictors <- vi_predictors[1:10]
  df <- vi[1:1000, ]

  # mixed types ----
  x <- vif_select(
    df = df,
    predictors = predictors
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% predictors)
  )

  testthat::expect_true(
    length(predictors) > length(x)
  )

  #custom preference order
  preference_order <- c(
    "swi_mean",
    "topo_elevation"
  )

  x <- vif_select(
    df = df,
    predictors = predictors,
    preference_order = preference_order
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% predictors)
  )

  testthat::expect_true(
    length(predictors) > length(x)
  )

  testthat::expect_true(
    all(preference_order[1] == x[1])
  )

  #automated preference order
  preference_order <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = predictors
  ) |>
    suppressMessages()

  x <- vif_select(
    df = df,
    predictors = predictors,
    preference_order = preference_order
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% predictors)
  )

  testthat::expect_true(
    length(predictors) > length(x)
  )

  testthat::expect_true(
    all(preference_order$predictor[1] == x[1])
  )

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  testthat::expect_message(
    x <- vif_select(
      df = df,
      predictors = predictors
    )
  )


  testthat::expect_true(
    is.na(x)
  )

  # edge cases ----

  #no df
  testthat::expect_error(
    x <- vif_select(
      df = NULL,
      predictors = NULL
    )
  )

  #predictors only
  testthat::expect_error(
    x <- vif_select(
      df = NULL,
      predictors = vi_predictors
    )
  )

  #few rows
  testthat::expect_error(
    x <- vif_select(
      df = vi[1, ],
      predictors = vi_predictors
    )
  )


  #no predictors
  x <- vif_select(
    df = df[, 1:5],
    predictors = NULL
  )

  testthat::expect_true(
    all(x %in% colnames(df)[1:5])
  )

  #single predictor
  predictors <- vi_predictors_numeric[1]

  testthat::expect_message(
    x <- vif_select(
      df = vi[1:1000, ],
      predictors = predictors
    )
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == predictors
  )

})
