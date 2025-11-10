testthat::test_that("`collinear()` works", {

  #load data
  data(
    vi_smol,
    vi_predictors_numeric,
    vi_predictors_categorical,
    vi_responses
  )

  #DEFAULT CALL ----
  #Error: collinear::collinear(): argument 'df' cannot be NULL
  testthat::expect_error(
    x <- collinear(),
    regexp = "'df' cannot be NULL"
  )

  #DF ONLY ----

  ##fewer than 3 rows ----
  testthat::expect_error(
    x <- collinear(
      df = vi_smol[1:2, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 3 rows"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  ##fewer than 10 rows ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  testthat::expect_warning(
    x <- collinear(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "the correlation matrix is singular and cannot be solved"
  ) |>
    suppressWarnings() |>
    suppressMessages()

  testthat::expect_message(
    x <- collinear(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric,
      max_vif = NULL
    ),
    regexp = "skipping VIF filtering"
  ) |>
    suppressWarnings() |>
    suppressMessages()

  ##fewer than 30 rows ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol[1:29, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  ##more than 30 rows ----

  #max_cor and max_vif NULL
  testthat::expect_error(
    x <- collinear(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = NULL
    ),
    regexp = "arguments 'max_cor' and 'max_vif' cannot be NULL at once"
  )

  #max_cor and max_vif invalid
  f_test <- function(){
    collinear(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = 2,
      max_vif = 20
    )
  }

  testthat::expect_message(
    x <- f_test(),
    regexp = "argument 'max_cor' is outside its valid range"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- f_test(),
    regexp = "argument 'max_vif' is outside its valid range"
  ) |>
    suppressMessages()

  #RESPONSE ----

  ##response only ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol[, c(vi_responses[1:2], vi_predictors_numeric)],
      responses = vi_responses[1:2],
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "processing response 'vi_numeric'"
  ) |>
    suppressMessages()

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
  testthat::expect_message(
    x <- collinear(
      df = vi,
      responses = NULL,
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "selected predictors"
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
      df = vi_smol,
      responses = NULL,
      predictors = vi_predictors_categorical[1:4],
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking 4 'predictors' from lower to higher multicollinearity"
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
    df = vi_smol,
    responses = NULL,
    predictors = c(vi_predictors_numeric[1:3], vi_predictors_categorical[1:2]),
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
  testthat::expect_message(
    x <- collinear(
      df = vi,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking 49 'predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()


  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_numeric$formulas))
  )


  ##categorical numeric ----
  testthat::expect_message(
    x <- collinear(
      df = vi,
      responses = "vi_categorical",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking 49 'predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()


  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##categorical categorical ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = "vi_categorical",
      predictors = vi_predictors_categorical[1:4],
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking 4 'predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  ##multiple responses ----
  #all selections are the same
  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = vi_responses,
      predictors = c(vi_responses, vi_predictors_numeric),
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "processing response"
  ) |>
    suppressMessages()

  #check that the given response does not appear in the selections
  for(i in names(x)){
    testthat::expect_true(
      !i %in% x[[i]]$selection
    )
  }

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
      df = vi_smol,
      responses = c("vi_numeric", "vi_categorical"),
      predictors = vi_predictors_categorical[1:4],
      encoding_method = "loo",
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    )
  }

  testthat::expect_message(
    x <- f_test(),
    regexp = "using response 'vi_numeric' to encode these categorical predictors"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- f_test(),
    regexp = "processing response 'vi_categorical'"
  ) |>
    suppressMessages()

  #check that a categorical predictor was converted to numeric for "vi_numeric"
  testthat::expect_true(
    !is.numeric(vi_smol[["soil_type"]]) &&
      !is.numeric(x$vi_categorical$df[["soil_type"]]) &&
      is.numeric(x$vi_numeric$df[["soil_type"]])
  )


  # PREFERENCE ORDER ----

  ## no target encoding ----

  ### invalid character vector ----
  preference_order <- c(
    "hola",
    "adios"
  )

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_order,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "invalid values in argument 'preference_order'"
  ) |>
    suppressMessages()

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_order,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "ranking 49 'predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()

  testthat::expect_true(
    !all(preference_order %in% x$vi_numeric$selection)
  )

  testthat::expect_true(
    all(vi_predictors_numeric %in% x$vi_numeric$preference_order$predictor)
  )

  ### valid character vector ----

  #uncorrelated, must show up in selection
  preference_order <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_order,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
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
    all(preference_order %in% x$vi_numeric$selection)
  )


  testthat::expect_true(
    all(vi_predictors_numeric %in% x$vi_numeric$preference_order$predictor)
  )


  ### valid dataframe ----
  preference_df <- preference_order(
    df = vi_smol,
    responses = c("vi_numeric", "vi_categorical"),
    predictors = vi_predictors_numeric,
    f = f_auto,
    quiet = TRUE
  )

  x <- collinear(
    df = vi_smol,
    responses = c("vi_numeric", "vi_categorical"),
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = preference_df,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    unique(x$vi_categorical$preference_order$response) == "vi_categorical"
  )

  ### invalid data frame ----
  preference_df <- data.frame(
    response = "hola",
    f = NA
  )

  testthat::expect_error(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_df,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "dataframe 'preference_order' must have these columns"
  )

  ### NULL response ----
  #same as above!
  preference <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = NULL,
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "ranking 47 'predictors' from lower to higher multicollinearity"
  ) |>
    suppressMessages()


  testthat::expect_true(
    all(preference %in% x$result$selection)
  )

  testthat::expect_true(
    preference[1] == x$result$preference_order$predictor[1]
  )

  testthat::expect_true(
    preference[2] == x$result$preference_order$predictor[2]
  )


  ### f_auto ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = f_auto,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "selected function 'f_numeric_glm"
  ) |>
    suppressMessages()

  testthat::expect_true(
    x$vi_numeric$selection[1] == x$vi_numeric$preference$predictor[1]
  )

  testthat::expect_true(
    x$vi_numeric$preference$f[1] == "f_numeric_glm"
  )

  ### f_numeric_rf ----
  x <- collinear(
    df = vi_smol,
    responses = "vi_numeric",
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = NULL,
    f = f_numeric_rf,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    x$vi_numeric$selection[1] == x$vi_numeric$preference$predictor[1]
  )


  ### bad function name ----
  my_f <- function(){return(NULL)}

  testthat::expect_error(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = my_f,
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "the function 'f' must have the argument 'df'"
  )

  ### character function name ----
  testthat::expect_error(
    x <- collinear(
      df = vi_smol,
      responses = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = NULL,
      f = "my_f",
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "must be a uquoted function name"
  )

  #passing a correlation matrix
  time_no_matrix <- system.time(
    expr = {
      x <- collinear(
        df = vi_smol,
        responses = "vi_numeric",
        predictors = vi_predictors_categorical[1:4],
        encoding_method = NULL,
        preference_order = NULL,
        f = NULL,
        max_cor = 0.75,
        max_vif = 5,
        quiet = TRUE
      )
    }
  )

  m <- cor_matrix(
    df = vi_smol,
    predictors = vi_predictors_categorical[1:4],
    quiet = TRUE
  )

  time_matrix <- system.time(
    expr = {
      x <- collinear(
        df = vi_smol,
        responses = "vi_numeric",
        predictors = vi_predictors_categorical[1:4],
        encoding_method = NULL,
        preference_order = NULL,
        f = NULL,
        max_cor = 0.75,
        max_vif = 5,
        quiet = TRUE,
        m = m
      )
    }
  )

  testthat::expect_true(
    time_no_matrix[3] > time_matrix[3]
  )


  #losing one column
  df <- vi_smol
  df$logical <- TRUE

  testthat::expect_message(
    x <- collinear(
      df = df,
      responses = "vi_numeric",
      predictors = c("logical", vi_predictors_numeric),
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "invalid logical variables due to constant values"
  ) |>
    suppressMessages()

  testthat::expect_true(
    !"logical" %in% colnames(x$vi_numeric$df)
  )



})
