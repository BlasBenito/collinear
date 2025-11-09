testthat::test_that("`vif_select()` works", {

  data(vi, vi_predictors, vi_predictors_numeric)

  testthat::expect_message(
    x <- vif_select(
      df = vi_smol,
      predictors = vi_predictors[1:10],
      quiet = FALSE
    ),
    regexp = "ranking 10 'predictors' from lower to higher multicollinearity"
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

  testthat::expect_message(
    y <- vif_select(
      df = vi_smol,
      predictors = x,
      quiet = FALSE
    ),
    regexp = "maximum VIF is <= 5, multicollinearity filtering is not required"
  ) |>
    suppressMessages()


  testthat::expect_true(
    all(x %in% y)
  )

  #custom preference order
  preference_order <- c(
    "swi_mean",
    "topo_elevation",
    "hola"
  )

  testthat::expect_message(
    x <- vif_select(
      df = vi_smol,
      predictors = vi_predictors[1:10],
      preference_order = preference_order
    ),
    regexp = "ranking 8 'predictors' from lower to higher multicollinearity"
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

  testthat::expect_true(
    all(preference_order[1] == x[1])
  )

  #automated preference order
  preference_order <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors[1:10],
    quiet = TRUE
  )

  x <- vif_select(
    df = vi_smol,
    predictors = vi_predictors[1:10],
    preference_order = preference_order,
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x)
  )

  testthat::expect_true(
    all(x %in% vi_predictors[1:10])
  )

  testthat::expect_true(
    length(vi_predictors[1:10]) > length(x)
  )

  testthat::expect_true(
    all(preference_order$predictor[1] == x[1])
  )


  # edge cases ----

  #no df
  testthat::expect_error(
    x <- vif_select(
      df = NULL,
      predictors = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  testthat::expect_error(
    x <- vif_select(
      df = vi_smol,
      predictors = vi_predictors,
      max_vif = NULL
    ),
    regexp = "argument 'max_vif' cannot be NULL"
  )

  #no predictors
  x <- vif_select(
    df = vi_smol[, 1:5],
    predictors = NULL,
    preference_order = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    all(x %in% colnames(vi_smol)[1:5])
  )

  #single predictor

  testthat::expect_message(
    x <- vif_select(
      df = vi_smol,
      predictors = vi_predictors_numeric[1]
    ),
    regexp = "only one valid predictor in 'predictors', skipping multicollinearity filtering"
  ) |>
    suppressMessages()


  testthat::expect_true(
    x == vi_predictors_numeric[1]
  )

})
