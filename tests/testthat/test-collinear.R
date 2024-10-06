testthat::test_that("`collinear()` works", {

  predictors <- vi_predictors[1:10]
  df <- vi[1:1000, ]

  # mixed types ----
  x <- collinear(
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

  x <- collinear(
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

  x <- collinear(
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

  #internal preference order
  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_numeric",
      predictors = predictors
    )
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


  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_binomial",
      predictors = predictors
    )
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

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_counts",
      predictors = predictors
    )
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

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_category",
      predictors = predictors
    )
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

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_factor",
      predictors = predictors
    )
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

  #disabling vif and target encoding
  predictors <- vi_predictors[1:10]

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_numeric",
      predictors = predictors,
      encoding_method = NULL,
      max_vif = NULL
    )
  ) |>
    suppressMessages()

  testthat::expect_message(
    y <- cor_select(
      df = df,
      predictors = predictors,
      preference_order = preference_order(
        df = df,
        response = "vi_numeric",
        predictors = predictors
      )
    )
  )

  testthat::expect_true(
    all(x == y)
  )

  #disabling cor and target encoding
  predictors <- vi_predictors_numeric[1:10]

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_numeric",
      predictors = predictors,
      encoding_method = NULL,
      max_cor = NULL
    )
  )

  testthat::expect_message(
    y <- vif_select(
      df = df,
      predictors = predictors,
      preference_order = preference_order(
        df = df,
        response = "vi_numeric",
        predictors = predictors
      )
    )
  )

  testthat::expect_true(
    all(x == y)
  )

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_factor",
      predictors = predictors
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% predictors)
  )

  testthat::expect_true(
    length(predictors) > length(x)
  )


  # edge cases ----

  #no df
  testthat::expect_error(
    x <- collinear(
      df = NULL,
      predictors = NULL
    )
  )

  #predictors only
  testthat::expect_error(
    x <- collinear(
      df = NULL,
      predictors = vi_predictors
    )
  )

  #few rows
  testthat::expect_error(
    x <- collinear(
      df = vi[1, ],
      predictors = vi_predictors
    )
  )


  #no predictors
  x <- collinear(
    df = df[, 1:5],
    predictors = NULL
  )

  testthat::expect_true(
    all(x %in% colnames(df)[1:5])
  )

  #single predictor
  predictors <- vi_predictors_numeric[1]

  x <- collinear(
    df = vi[1:1000, ],
    predictors = predictors
  )

  testthat::expect_true(
    x == predictors
  )

})
