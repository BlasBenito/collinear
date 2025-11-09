testthat::test_that("`identify_zero_variance_variables()` works", {

  data(vi_smol, vi_predictors, vi_predictors_categorical)

  vi_smol$zv_1 <- 1

  vi_smol$zv_2 <- runif(
    n = nrow(vi_smol),
    min = 0,
    max = 1e-04
    )

  testthat::expect_message(
    x <- identify_zero_variance_variables(
      df = vi_smol,
      predictors = c(
        vi_predictors,
        "zv_1",
        "zv_2"
      )
    ),
    regexp = "invalid predictors due to near-zero variance"
  )


  testthat::expect_true(
    is.character(x),
  )

  testthat::expect_true(
    length(x) == 2
  )

  testthat::expect_true(
    all(c("zv_1", "zv_2") %in% x)
  )

  ##
  testthat::expect_message(
    x <- identify_zero_variance_variables(
      df = vi_smol,
      responses = c(
        vi_predictors,
        "zv_1",
        "zv_2"
      )
    ),
    regexp = "invalid responses due to near-zero variance"
  )


  testthat::expect_true(
    is.character(x),
  )

  testthat::expect_true(
    length(x) == 2
  )

  testthat::expect_true(
    all(c("zv_1", "zv_2") %in% x)
  )

  ##
  testthat::expect_message(
    x <- identify_zero_variance_variables(
      df = vi_smol,
      responses = c(
        "zv_1",
        "zv_2"
      ),
      predictors = vi_predictors
    ),
    regexp = "invalid variables due to near-zero variance"
  )


  testthat::expect_true(
    is.character(x),
  )

  testthat::expect_true(
    length(x) == 2
  )

  testthat::expect_true(
    all(c("zv_1", "zv_2") %in% x)
  )


  ##
  x <- identify_zero_variance_variables(
    df = vi_smol,
    predictors = vi_predictors_numeric
  )

  testthat::expect_true(
    is.null(x)
  )

  x <- identify_zero_variance_variables(
    df = vi_smol,
    predictors = vi_predictors
  )

  #without predictors
  testthat::expect_error(
    x <- identify_zero_variance_variables(
      df = vi_smol
    ),
    regexp = "there are no variables to identify"
  )


  #edge cases
  testthat::expect_error(
    x <- identify_zero_variance_variables(
      df = NULL
    ),
    regexp = "argument 'df' cannot be NULL"
  )

  #non overlapping predictors?
  testthat::expect_error(
    x <- identify_zero_variance_variables(
      df = vi_smol,
      predictors = c("hola", "adios")
    ),
    regexp = "there are no variables to identify"
    ) |>
    suppressWarnings()

  x <- identify_zero_variance_variables(
    df = vi_smol,
    predictors = vi_predictors_categorical
  )

  testthat::expect_null(
    x
  )


})

