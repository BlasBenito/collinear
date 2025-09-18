testthat::test_that("`cor_df()` works", {

  data(vi_smol, vi_predictors, vi_predictors_categorical)

  #mixed types
  cor.df <- cor_df(
    df = vi_smol,
    predictors = vi_predictors[1:10]
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
  cor.df <- cor_df(
    df = vi_smol,
    predictors = vi_predictors_categorical[1:5]
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
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #predictors only
  testthat::expect_error(
    cor.df <- cor_df(
      df = NULL,
      predictors = vi_predictors
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #few rows
  testthat::expect_error(
    cor.df <- cor_df(
      df = vi_smol[1, ],
      predictors = vi_predictors,
      quiet = TRUE
    ),
    regexp = "argument 'df' has fewer than 3 rows"
  )


  #no predictors
  cor.df <- cor_df(
    df = vi_smol[, 1:5],
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
  testthat::expect_message(
    cor.df <- cor_df(
      df = vi_smol,
      predictors = vi_predictors[1],
      quiet = FALSE
    ),
    regexp = "only one predictor"
  )


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
