testthat::test_that("`collinear()` works", {

  data(vi, vi_predictors)

  predictors <- vi_predictors[1:20]
  df <- vi[1:1000, ]

  # several responses ----
  responses <- c(
    "vi_numeric",
    "vi_counts",
    "vi_binomial",
    "vi_categorical",
    "vi_factor"
  )

  #external preference order
  preference_list <- preference_order(
    df = df,
    response = responses,
    predictors = predictors,
    f = NULL,
    warn_limit = NULL,
    quiet = TRUE
  )

  x <- collinear(
    df = df,
    response = responses,
    predictors = predictors,
    preference_order = preference_list,
    quiet = TRUE
  )

  testthat::expect_true(
    is.list(x)
  )

  testthat::expect_true(
    all(names(x) %in% responses)
  )

  #preference order auto
  #should give the same results, but somehow it does not
  y <- collinear(
    df = df,
    response = responses,
    predictors = predictors,
    preference_order = "auto",
    quiet = TRUE
  )

  testthat::expect_true(
    is.list(y)
  )

  testthat::expect_true(
    all(names(y) %in% responses)
  )


  # mixed types ----
  x <- collinear(
    df = df,
    predictors = predictors,
    quiet = TRUE
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
    preference_order = preference_order,
    quiet = TRUE
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
    predictors = predictors,
    warn_limit = NULL,
    quiet = TRUE
  )

  x <- collinear(
    df = df,
    predictors = predictors,
    preference_order = preference_order,
    quiet = TRUE
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
      predictors = predictors,
      quiet = FALSE
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

  #disabling vif and target encoding
  predictors <- vi_predictors[1:10]

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_numeric",
      predictors = predictors,
      encoding_method = NULL,
      max_vif = NULL,
      quiet = FALSE
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
        predictors = predictors,
        warn_limit = NULL
      ),
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    all(x == y)
  )

  #disabling cor and target encoding
  predictors <- vi_predictors_numeric[1:10]

  preference_order <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = predictors
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_numeric",
      predictors = predictors,
      preference_order = preference_order,
      encoding_method = NULL,
      max_cor = NULL,
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  testthat::expect_message(
    y <- vif_select(
      df = df,
      predictors = predictors,
      preference_order = preference_order,
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    all(x == y)
  )

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  testthat::expect_message(
    x <- collinear(
      df = df,
      response = "vi_factor",
      predictors = predictors,
      quiet = FALSE
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
      predictors = vi_predictors,
      quiet = TRUE
    )
  )

  #no predictors
  x <- collinear(
    df = df[, 1:5],
    predictors = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    all(x %in% colnames(df)[1:5])
  )

  #single predictor
  predictors <- vi_predictors_numeric[1]

  testthat::expect_message(
    x <- collinear(
      df = vi[1:1000, ],
      predictors = predictors
    )
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == predictors
  )

})
