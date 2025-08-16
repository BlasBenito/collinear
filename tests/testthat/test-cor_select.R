testthat::test_that("`cor_select()` works", {

  data(vi, vi_predictors)

  predictors <- vi_predictors[1:10]
  df <- vi[1:1000, ]

  # mixed types ----
  testthat::expect_message(
    x <- cor_select(
      df = df,
      predictors = predictors,
      quiet = FALSE
    ),
    regexp = "ranking predictors"
  ) |>
    suppressMessages()

  x <- cor_select(
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

  x <- cor_select(
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
    preference_order[1] == x[1]
  )

  #automated preference order
  preference_order <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = predictors,
    quiet = TRUE,
    warn_limit = NULL
  )

  x <- cor_select(
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

  # categorical only ----
  predictors <- vi_predictors_categorical[1:5]

  x <- cor_select(
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

  # edge cases ----

  #no df
  testthat::expect_error(
    x <- cor_select(
      df = NULL,
      predictors = NULL
    )
  )

  #predictors only
  testthat::expect_error(
    x <- cor_select(
      df = NULL,
      predictors = vi_predictors
    )
  )

  #few rows
  testthat::expect_error(
    x <- cor_select(
      df = vi[1:2, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 3 rows"
  )

  testthat::expect_warning(
    x <- cor_select(
      df = vi[1:9, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- cor_select(
      df = vi[1:29, ],
      predictors = vi_predictors
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()


  #no predictors
  x <- cor_select(
    df = df[, 1:5],
    predictors = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    all(x %in% colnames(df)[1:5])
  )

  #single predictor
  predictors <- vi_predictors_categorical[1]

  testthat::expect_message(
    x <- cor_select(
      df = vi[1:1000, ],
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
    length(predictors) == length(x)
  )

})
