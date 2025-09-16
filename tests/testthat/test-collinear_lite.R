testthat::test_that("`collinear_lite()` works", {

  data(
    vi,
    vi_smol,
    vi_predictors,
    vi_predictors_numeric,
    vi_predictors_categorical
  )

  #DEFAULT CALL ----
  #Error: collinear::collinear(): argument 'df' cannot be NULL
  testthat::expect_error(
    x <- collinear_lite(),
    regexp = "'df' cannot be NULL"
  )

  #DF ONLY ----

  ##fewer than 10 rows ----
  testthat::expect_warning(
    x <- collinear_lite(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  ##fewer than 30 rows ----
  testthat::expect_message(
    x <- collinear_lite(
      df = vi_smol[1:11, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()

  ##more than 30 rows ----

  #max_cor and max_vif
  testthat::expect_error(
    x <- collinear_lite(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = NULL
    ),
    regexp = "arguments 'max_cor' and 'max_vif' cannot be NULL"
  ) |>
    suppressMessages()


  #PREDICTORS ----

  ##numeric predictors ----
  x <- collinear_lite(
    df = vi,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x) && all(x %in% vi_predictors_numeric)
  )

  ##categorical predictors ----
  testthat::expect_message(
    x <- collinear_lite(
      df = vi_smol,
      predictors = vi_predictors_categorical[1:5],
      quiet = FALSE
    ),
    regexp = "no numeric predictors available for VIF filtering"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(x) && all(x %in% vi_predictors_categorical)
  )


  ##mixed predictors ----
  x <- collinear_lite(
    df = vi_smol,
    predictors = c(vi_predictors_numeric[1:3], vi_predictors_categorical[1:3]),
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x) && all(x %in% vi_predictors)
  )

  #PREDICTORS + RESPONSE ----

  ##numeric numeric ----
  x <- collinear_lite(
    df = vi,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    is.character(x) && all(x %in% vi_predictors_numeric)
  )

  # PREFERENCE ORDER ----

  ## no target encoding ----

  ### invalid character vector ----
  x <- collinear_lite(
    df = vi_smol,
    predictors = vi_predictors_numeric,
    preference_order = "soil_nitrogen",
    quiet = TRUE
  )

  testthat::expect_true(
    x[1] == "soil_nitrogen"
  )

})
