testthat::test_that("`cor_select()` works", {
  testthat::skip_on_cran()
  data(vi_smol, vi_predictors)

  # mixed types ----
  testthat::expect_message(
    x <- cor_select(
      df = vi_smol,
      predictors = vi_predictors[1:10],
      quiet = FALSE
    ),
    regexp = "from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors[1:10])
  )

  testthat::expect_true(
    length(vi_predictors[1:10]) > length(x)
  )

  #custom preference order
  preference_order <- c(
    "swi_mean",
    "topo_elevation"
  )

  testthat::expect_message(
    x <- cor_select(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      preference_order = preference_order,
      quiet = FALSE
    ),
    regexp = paste0(
      "ranking ",
      length(vi_predictors_numeric) - length(preference_order),
      " 'predictors' from lower to higher multicollinearity"
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    length(vi_predictors_numeric) > length(x)
  )

  testthat::expect_true(
    preference_order[1] == x[1]
  )

  #automated preference order
  preference_order_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  x <- cor_select(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    preference_order = preference_order_df,
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    length(vi_predictors_numeric) > length(x)
  )

  testthat::expect_true(
    all(preference_order_df$predictor[1] == x[1])
  )

  # categorical only ----
  x <- cor_select(
    df = vi_smol,
    predictors = vi_predictors_categorical[1:4],
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors_categorical[1:4])
  )

  testthat::expect_true(
    length(vi_predictors_categorical[1:4]) > length(x)
  )

  testthat::expect_message(
    y <- cor_select(
      df = vi_smol,
      predictors = x,
      quiet = FALSE
    ),
    regexp = "maximum pairwise correlation is <="
  ) |>
    suppressMessages()

  # edge cases ----

  #no df
  testthat::expect_error(
    x <- cor_select(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #predictors only
  testthat::expect_error(
    x <- cor_select(
      df = NULL,
      predictors = vi_predictors
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #few rows
  testthat::expect_error(
    x <- cor_select(
      df = vi_smol[1, ],
      predictors = vi_predictors
    ),
    regexp = "argument 'df' has fewer than 3 rows"
  )

  #null max cor
  testthat::expect_error(
    x <- cor_select(
      df = vi_smol[, 1:5],
      predictors = NULL,
      max_cor = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'max_cor' cannot be NULL"
  )

  #no predictors
  x <- cor_select(
    df = vi_smol[, 1:5],
    predictors = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    all(x %in% colnames(vi_smol)[1:5])
  )

  #single predictor
  testthat::expect_message(
    x <- cor_select(
      df = vi_smol,
      predictors = vi_predictors_categorical[1]
    ),
    regexp = "only one valid predictor"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors_categorical[1])
  )

  testthat::expect_true(
    length(vi_predictors_categorical[1]) == length(x)
  )
})
