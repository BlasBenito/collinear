testthat::test_that("`collinear_auto()` works", {

  data(
    vi,
    vi_smol,
    vi_predictors,
    vi_predictors_numeric,
    vi_predictors_categorical,
    vi_responses
  )

  #DEFAULT CALL ----
  #Error: collinear::collinear(): argument 'df' cannot be NULL
  testthat::expect_error(
    x <- collinear_auto(),
    regexp = "'df' cannot be NULL"
  )

  #DF ONLY ----

  ##fewer than 10 rows ----
  testthat::expect_warning(
    x <- collinear_auto(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  ##fewer than 30 rows ----
  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol[1:11, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()

  ##more than 30 rows ----

  #max_cor and max_vif
  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = NULL
    ),
    regexp = "autoconfiguring 'max_cor'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = NULL
    ),
    regexp = "autoconfiguring 'max_vif'"
  ) |>
    suppressMessages()

  #RESPONSE ----

  ##response only ----
  x <- collinear_auto(
    df = vi_smol[, c(vi_responses[1:2], vi_predictors_numeric)],
    responses = vi_responses[1:2],
    quiet = TRUE
  )

  testthat::expect_true(
    all(vi_responses[1:2] %in% names(x))
  )

  #check that the given response is never in selections
  for(i in vi_responses[1:2]){
    testthat::expect_true(
      !i %in% x[[i]]$selection
    )
  }


  #PREDICTORS ----

  ##numeric predictors ----
  x <- collinear_auto(
    df = vi,
    responses = NULL,
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  #test lack of formulas because there is no response
  testthat::expect_true(
    !any(c("formula", "formulas") %in% names(x))
  )

  ##categorical predictors ----
  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol,
      responses = NULL,
      predictors = vi_predictors_categorical[1:5],
      quiet = FALSE
    ),
    regexp = "max_vif = NULL"
  ) |>
    suppressMessages()

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  #test lack of formulas because there is no response
  testthat::expect_true(
    !any(c("formula", "formulas") %in% names(x))
  )

  ##mixed predictors ----
  x <- collinear_auto(
    df = vi_smol,
    responses = NULL,
    predictors = c(vi_predictors_numeric[1:3], vi_predictors_categorical[1:3]),
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  #test lack of formulas because there is no response
  testthat::expect_true(
    !any(c("formula", "formulas") %in% names(x))
  )

  #PREDICTORS + RESPONSE ----

  ##numeric numeric ----
  x <- collinear_auto(
    df = vi,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_numeric$formulas))
  )


  ##categorical numeric ----
  x <- collinear_auto(
    df = vi_smol,
    responses = "vi_categorical",
    predictors = vi_predictors_numeric[1:10],
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##categorical categorical ----
  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol,
      responses = "vi_categorical",
      predictors = vi_predictors_categorical,
      quiet = FALSE
    ),
    regexp = "max_vif = NULL"
  ) |>
    suppressMessages()

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##multiple responses ----
  #all selections are the same
  testthat::expect_message(
    x <- collinear_auto(
      df = vi_smol,
      responses = vi_responses,
      predictors = c(vi_predictors_numeric[1:10], vi_predictors_categorical[1:2]),
      quiet = FALSE
    ),
    regexp = "processing response"
  ) |>
    suppressMessages()

  #check that all responses are in x
  testthat::expect_true(
    all(vi_responses %in% names(x))
  )

  #checking formulas
  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_numeric$formulas))
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_counts$formulas))
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_binomial$formulas))
  )

  testthat::expect_true(
    all(c("classification") %in% names(x$vi_factor$formulas))
  )

  testthat::expect_true(
    all(c("classification") %in% names(x$vi_categorical$formulas))
  )


  #TARGET ENCODING ----
  f_test <- function(){
    collinear_auto(
      df = vi_smol,
      responses = c("vi_numeric", "vi_categorical"),
      predictors = vi_predictors_categorical,
      quiet = FALSE
    )
  }

  testthat::expect_message(
    x <- f_test(),
    regexp = "using response 'vi_numeric' to encode"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- f_test(),
    regexp = "argument 'response' is categorical"
  ) |>
    suppressMessages()

  #check that a categorical predictor was converted to numeric for "vi_numeric"

  varname <- x$vi_numeric$selection[1]

  testthat::expect_true(
    !is.numeric(vi_smol[[varname]]) &&
      !is.numeric(x$vi_categorical$df[[varname]]) &&
      is.numeric(x$vi_numeric$df[[varname]])
  )


  # PREFERENCE ORDER ----

  ## no target encoding ----

  ### invalid character vector ----
  x <- collinear_auto(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric,
    quiet = TRUE
  )

  testthat::expect_true(
    all(x$vi_numeric$preference$df$predictor %in% vi_predictors_numeric)
  )

  testthat::expect_true(
    is.null(x$arguments$preference_order)
  )

})
