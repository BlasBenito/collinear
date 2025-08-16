
testthat::test_that("`validate_arg_response()` works", {

  data(vi)

  df <- vi[1:1000, ]

  #no arguments
  testthat::expect_error(
    response <- validate_arg_response(
      df = NULL,
      response = NULL,
      function_name = NULL,
      quiet = FALSE
    )
  )


  #valid use case
  response <- validate_arg_response(
    df = vi,
    response = "vi_numeric"
  )

  testthat::expect_true(
    attributes(response)$validated
  )

  #response is NULL
  response <- validate_arg_response(
    df = vi,
    response = NULL
  )

  testthat::expect_true(
    is.null(response)
  )

  #response not in df
  testthat::expect_warning(
    response <- validate_arg_response(
      df = vi,
      response = "hola"
    )
  ) |>
    suppressWarnings()

  testthat::expect_true(
    is.null(response)
  )

  #all null
  testthat::expect_error(
    response <- validate_arg_response(
      df = NULL,
      response = NULL
    )
  )

  #constant
  vi$constant <- 1

  testthat::expect_warning(
    response <- validate_arg_response(
      df = vi,
      response = "constant"
    )
  )

  testthat::expect_true(
    is.null(response)
  )

  #non-numeric
  vi$character <- "hola"

  testthat::expect_warning(
    response <- validate_arg_response(
      df = vi,
      response = "character"
    )
  )

  testthat::expect_true(
    is.null(response)
  )

})
