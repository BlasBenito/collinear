testthat::test_that("`collinear()` works", {

  data(vi, vi_predictors)

  predictors_misc <- c("koppen_zone", "region", "continent", "topo_slope", "topo_elevation", "growing_season_temperature", "soil_soc", "country_gdp", "country_name", "koppen_group")

  predictors_numeric <- vi_predictors_numeric[1:5]

  predictors_categorical <- vi_predictors_categorical[1:5]

  vi.smol <- vi[1:1000, ]

  #response types
  responses <- c(
    "vi_numeric",
    "vi_counts",
    "vi_binomial",
    "vi_categorical",
    "vi_factor"
  )

  future::plan(
    future::multisession,
    workers = 3
  )

  #DEFAULT CALL ----
  #Error: collinear::collinear(): argument 'df' cannot be NULL
  testthat::expect_error(
    x <- collinear(),
    regexp = "'df' cannot be NULL"
  )

  #DF ONLY ----

  ##fewer than 10 rows ----
  testthat::expect_warning(
    x <- collinear(
      df = vi.smol[1:5, 1:5]
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  ##fewer than 30 rows ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol[1:11, 1:10]
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()

  ##more than 30 rows ----

  #max_cor and max_vif NULL
  testthat::expect_error(
    x <- collinear(
      df = vi.smol[, 1:10],
      max_cor = NULL,
      max_vif = NULL
    )
  )

  #max_cor and max_vif invalid
  f_test <- function(){
    collinear(
      df = vi.smol[, 1:5],
      max_cor = 2,
      max_vif = 20
    )
  }

  testthat::expect_message(
    x <- f_test(),
    regexp = "argument 'max_cor' is outside its recommended range"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- f_test(),
    regexp = "argument 'max_vif' is outside its recommended range"
  ) |>
    suppressMessages()

  #RESPONSE ----

  ##response only ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol[, c(responses, predictors_misc)],
      response = responses,
      predictors = NULL,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking predictors from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    all(responses %in% names(x))
  )

  #check that the given response is never in selections
  for(i in responses){
    testthat::expect_true(
      !i %in% x[[i]]$selection
    )
  }


  #PREDICTORS ----

  ##numeric predictors ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = NULL,
      predictors = predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking predictors from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  #test lack of formulas because there is no response
  testthat::expect_true(
    !any(c("formula", "formulas") %in% names(x))
  )

  ##categorical predictors ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = NULL,
      predictors = predictors_categorical,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "no numeric columns in argument"
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
  x <- collinear(
    df = vi.smol,
    response = NULL,
    predictors = predictors_misc,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
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
  x <- collinear(
    df = vi.smol,
    response = "vi_numeric",
    predictors = predictors_numeric,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$formulas))
  )


  ##categorical numeric ----
  x <- collinear(
    df = vi.smol,
    response = "vi_categorical",
    predictors = predictors_numeric,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##categorical categorical ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = "vi_categorical",
      predictors = predictors_categorical,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "no numeric columns in argument"
  ) |>
    suppressMessages()

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##multiple responses ----
  #all selections are the same
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = responses,
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "processing response"
  ) |>
    suppressMessages()

  #check that all responses are in x
  testthat::expect_true(
    all(responses %in% names(x))
  )

  #check for identical selections because encoding_method and preference_order are NULL
  selections <- lapply(
    X = x,
    FUN = function(x) x$selection
  ) |>
    unlist() |>
    table() |>
    as.data.frame()

  testthat::expect_true(
    all(selections$Freq == length(responses))
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
    collinear(
      df = vi.smol,
      response = c("vi_numeric", "vi_categorical"),
      predictors = predictors_categorical,
      encoding_method = "loo",
      preference_order = NULL,
      f = NULL,
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
  testthat::expect_true(
    !is.numeric(vi.smol[["soil_type"]]) &&
      !is.numeric(x$vi_categorical$df[["soil_type"]]) &&
      is.numeric(x$vi_numeric$df[["soil_type"]])
  )


  #PREFERENCE ORDER ----

  ## no target encoding ----

  ### invalid character vector ----
  preference_order <- c(
    "hola",
    "adios"
  )

  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = preference_order,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "does not contain valid column names in"
  ) |>
    suppressMessages()

  testthat::expect_true(
    !all(preference_order %in% x$selection)
  )

  testthat::expect_true(
    is.null(x$arguments$preference_order)
  )

  ### valid character vector ----

  #uncorrelated, must show up in selection
  preference_order <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  x <- collinear(
    df = vi.smol,
    response = "vi_numeric",
    predictors = predictors_misc,
    encoding_method = NULL,
    preference_order = preference_order,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all(preference_order %in% x$selection)
  )

  testthat::expect_true(
    all(preference_order %in% x$arguments$preference_order)
  )

  ### NULL response ----
  #same as above!
  preference_order <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  x <- collinear(
    df = vi.smol,
    response = NULL,
    predictors = predictors_misc,
    encoding_method = NULL,
    preference_order = preference_order,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all(preference_order %in% x$selection)
  )

  testthat::expect_true(
    all(preference_order %in% x$arguments$preference_order)
  )

  testthat::expect_true(
    x$selection[1] == x$arguments$preference_order[1]
  )

  ### f_auto ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = f_auto,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "f_auto"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$selection[1] == x$arguments$preference_order[1]
  )

  testthat::expect_true(
    !is.null(x$arguments$f)
  )

  testthat::expect_true(
    x$arguments$f == "f_r2_rf"
  )

  ### f_r2_rf ----
  testthat::expect_message(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = f_r2_rf,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "f_r2_rf"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$selection[1] == x$arguments$preference_order[1]
  )

  testthat::expect_true(
    !is.null(x$arguments$f)
  )

  testthat::expect_true(
    x$arguments$f == "f_r2_rf"
  )

  ### bad function name
  my_f <- function(){return(NULL)}

  testthat::expect_error(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = my_f,
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "to receive a data frame with the column names"
  )

  ### character function name
  testthat::expect_error(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = "my_f",
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "must be a uquoted function name"
  )

  ###wrong function type
  testthat::expect_error(
    x <- collinear(
      df = vi.smol,
      response = "vi_numeric",
      predictors = predictors_misc,
      encoding_method = NULL,
      preference_order = NULL,
      f = f_v, #requires character response
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "must be of class 'character'"
  )



})
