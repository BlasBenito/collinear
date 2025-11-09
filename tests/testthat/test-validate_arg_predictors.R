testthat::test_that("`validate_arg_predictors()` works", {

  data(
    vi_smol,
    vi_predictors_numeric,
    vi_predictors,
    vi_predictors_categorical
    )

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors(
      df = NULL,
      responses = NULL,
      predictors = NULL,
      quiet = FALSE
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #without predictors
  #without response
  #must contain only numeric columns
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    length(predictors) == ncol(vi_smol)
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi_smol))
  )

  #validated predictors from previous test
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = NULL,
      predictors = predictors,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    length(predictors) == ncol(vi_smol)
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi_smol))
  )


  #wrong predictors
  testthat::expect_warning(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = NULL,
      predictors = c("hola", "adios"),
      quiet = FALSE
    ),
    regexp = " none of the 'predictors' are column names of 'df'"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.null(predictors)
  )

  #wrong and right predictors
  testthat::expect_message(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = NULL,
      predictors = c("hola", "adios", vi_predictors),
      quiet = FALSE
    ),
    regexp = "these 'predictors' are not column names of 'df' and will be ignored"
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.character(predictors)
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi_smol))
  )

  #without predictors
  #with response
  #must contain all df columns but the response
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = "vi_numeric",
      quiet = FALSE
    )
  )

  testthat::expect_true(
    !("vi_numeric" %in% predictors)
  )

  #with predictors
  #with response
  #must contain all predictors
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    all(predictors %in% vi_predictors)
  )


})
