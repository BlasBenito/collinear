testthat::test_that("`validate_arg_df()` works", {

  data(vi, vi_predictors)

  #no arguments
  testthat::expect_error(
    df <- validate_arg_df(
      df = NULL,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_message(
    df <- validate_arg_df(
      df = vi,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_message(
    df <- validate_arg_df(
      df = vi,
      response = NULL,
      predictors = vi_predictors,
      quiet = FALSE
    )
  )


  #normal usage
  testthat::expect_message(
    df <- validate_arg_df(
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_true(attributes(df)$validated)

  testthat::expect_true(
    all(c("vi_numeric", vi_predictors) %in% colnames(df))
  )

  #only one predictor
  df <- validate_arg_df(
    df = vi,
    response = NULL,
    predictors = vi_predictors[1],
    quiet = FALSE
  )

  testthat::expect_true(attributes(df)$validated)

  testthat::expect_true(
    all(vi_predictors[1] %in% colnames(df))
  )

})

testthat::test_that("`validate_arg_predictors()` works", {

  data(vi)

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors(
      df = NULL,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #without response
  #must contain only numeric columns
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi,
      response = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    length(predictors) == ncol(vi)
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi))
  )

  #without predictors
  #with response
  #must contain all df columns but the response
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi,
      response = "vi_numeric",
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
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    all(predictors %in% vi_predictors)
  )

  #with constant predictors
  vi$zero_variance <- 1
  vi$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors(
      df = vi,
      response = "vi_numeric",
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors(
      df = vi,
      response = "vi_numeric",
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = TRUE
    )
  )

  testthat::expect_true(
    !"zero_variance" %in% predictors
  )

  testthat::expect_true(
    !"constant" %in% predictors
  )


})



testthat::test_that("`validate_arg_predictors_vif()` works", {

  data(vi)

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_vif(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #must contain only numeric columns
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = NULL,
      quiet = TRUE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    attributes(predictors)$validated_vif
  )

  testthat::expect_true(
    all(predictors %in% identify_predictors_numeric(
      df = vi
    ))
  )

  #not enough rows
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi[1:10, ],
      predictors = NULL,
      quiet = FALSE
    )
  )

  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = "hola",
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.null(predictors)
  )

  #only one predictor
  #predictor not in df
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = "vi_numeric",
      quiet = FALSE
    )
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = "vi_numeric",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    length(predictors) == 1
  )

  #with constant predictors
  vi$zero_variance <- 1
  vi$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_vif(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = TRUE
    )
  )

  testthat::expect_true(
    !"zero_variance" %in% predictors
  )

  testthat::expect_true(
    !"constant" %in% predictors
  )


})



testthat::test_that("`validate_arg_predictors_cor()` works", {

  data(vi)

  #no arguments
  testthat::expect_error(
    predictors <- validate_arg_predictors_cor(
      df = NULL,
      predictors = NULL,
      quiet = FALSE
    )
  )

  #without predictors
  #must contain all columns
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(predictors)$validated
  )

  testthat::expect_true(
    attributes(predictors)$validated_cor
  )

  testthat::expect_true(
    all(predictors %in% colnames(vi))
  )

  #not enough rows
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi[1:9, ],
      predictors = NULL,
      quiet = FALSE
    )
  )

  #predictor not in df
  testthat::expect_warning(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "hola",
      quiet = TRUE
    )
  ) |>
    suppressMessages()

  testthat::expect_true(
    is.null(predictors)
  )

  #only one predictor
  #predictor not in df
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "vi_numeric",
      quiet = FALSE
    )
  )

  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = "vi_numeric",
      quiet = TRUE
    )
  )

  testthat::expect_true(
    length(predictors) == 1
  )

  #with constant predictors
  vi$zero_variance <- 1
  vi$constant <- "hola"

  #with quiet = FALSE
  testthat::expect_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = FALSE
    )
  ) |>
    suppressMessages()

  #with quiet = TRUE
  testthat::expect_no_message(
    predictors <- validate_arg_predictors_cor(
      df = vi,
      predictors = c(vi_predictors, "zero_variance", "constant"),
      quiet = TRUE
    )
  )

  testthat::expect_true(
    !"zero_variance" %in% predictors
  )

  testthat::expect_true(
    !"constant" %in% predictors
  )


})


testthat::test_that("`validate_arg_response()` works", {

  data(vi)

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



testthat::test_that("`validate_arg_preference_order()` works", {

  data(vi, vi_predictors)

  #no arguments
  testthat::expect_error(
    preference_order <- validate_arg_preference_order(
      predictors = NULL,
      preference_order = NULL,
      preference_order_auto = NULL,
      function_name = NULL,
      quiet = FALSE
    )
  )

  #predictors not validated
  testthat::expect_error(
    preference_order <- validate_arg_preference_order(
      predictors = vi_predictors,
      preference_order = vi_predictors,
      preference_order_auto = vi_predictors,
      function_name = NULL,
      quiet = FALSE
    )
  )



  #missing predictor
  predictors <- validate_arg_predictors(
    df = vi,
    predictors = vi_predictors
  )

  testthat::expect_message(
    preference_order <- validate_arg_preference_order(
      predictors = predictors,
      preference_order = c(vi_predictors, "hola"),
      preference_order_auto = vi_predictors,
      function_name = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(preference_order)$validated
  )

  testthat::expect_true(
    all(preference_order %in% vi_predictors)
  )

  #using data frame from preference_order()
  testthat::expect_no_message(
    preference_order <- validate_arg_preference_order(
      predictors = predictors,
      preference_order = data.frame(
        predictor = vi_predictors,
        preference = 1
      ),
      preference_order_auto = vi_predictors,
      function_name = NULL,
      quiet = FALSE
    )
  )

  testthat::expect_true(
    attributes(preference_order)$validated
  )

  testthat::expect_true(
    all(preference_order %in% vi_predictors)
  )


})
