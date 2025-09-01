testthat::test_that("`collinear()` works", {

  #CODE EXAMPLES ----

#'   data(
#'     vi_smol,
#'     vi_predictors_numeric
#'   )
#'
#'   ##OPTIONAL: parallelization setup
#'   # future::plan(
#'   #   future::multisession,
#'   #   workers = 2
#'   # )
#'
#'   ##OPTIONAL: progress bar
#'   ##does not work in R examples
#'   #progressr::handlers(global = TRUE)
#'
#'   ##minimal setup
#'   ##--------------------------
#'   ##all columns in df are filtered
#'   ##uses numeric columns only to speed up example
#'   x <- collinear(
#'     df = vi_smol[, vi_predictors_numeric]
#'   )
#'
#'   names(x)
#'
#'   #print full object
#'   print(x)
#'
#'   #print selection only
#'   summary(x)
#'
#'   #get selection vector
#'   x$result$selection
#'
#'   #validated arguments are stored as well
#'   x$arguments
#'
#'   ##using predictors
#'   ##--------------------------
#'   ## - numeric predictors only
#'   ## - ordered by their mutual collinearity
#'   ## - not enough rows for a full VIF analysis
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric
#'   )
#'
#'
#'   ##disable VIF analysis
#'   ##--------------------------
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric,
#'     max_vif = NULL
#'   )
#'
#'
#'   ##disable correlation analysis
#'   ##--------------------------
#'   x <- collinear(
#'     df = vi_smol,
#'     predictors = vi_predictors_numeric,
#'     max_cor = NULL
#'   )
#'
#'
#'   ##automatic preference order
#'   ##--------------------------
#'   ## - rank predictors by R-squared with response (see f_functions() for more options)
#'   x <- collinear(
#'     df = vi_smol,
#'     response = "vi_numeric",
#'     predictors = vi_predictors_numeric,
#'     f = f_r2_pearson
#'   )
#'
#'   ##results for the given response
#'   x$vi_numeric
#'
#'   ##response name is used instead of 'result'
#'   x$vi_numeric$selection
#'
#'   ##formula for linear model
#'   x$vi_numeric$formulas$linear
#'
#'   ##formula for gam model
#'   x$vi_numeric$formulas$smooth
#'
#'   ##data frame with selected columns
#'   colnames(x$vi_numeric$df)
#'
#'   #preference dataframe in results
#'   x$vi_numeric$preference$df
#'
#'
#'   ##manual preference order
#'   ##--------------------------
#'   ## - rank predictors by order in vector 'preference_order'
#'   x <- collinear(
#'     df = vi_smol,
#'     response = "vi_numeric",
#'     predictors = vi_predictors_numeric,
#'     preference_order = c(
#'       "swi_mean",
#'       "soil_temperature_mean",
#'       "growing_season_length",
#'       "rainfall_mean"
#'     )
#'   )
#'
#'   #preference dataframe in results
#'   x$vi_numeric$preference$df
#'
#'   ##missing predictors in preference_order due to multicollinearity with predictors with a higher preference
#'   ##predictors not in preference order ranked by their collinearity with other predictors
#'   summary(x)
#'
#'
#'   ##several responses
#'   ##--------------------------------
#'   ## - automatic selection of preference order function with f_auto()
#'   x <- collinear(
#'     df = vi_smol,
#'     response = c(
#'       "vi_numeric",
#'       "vi_binomial"
#'     ),
#'     predictors = vi_predictors_numeric,
#'     f = f_auto
#'   )
#'
#'   ##one object per response
#'   x$vi_numeric$selection
#'   x$vi_binomial$selection
#'
#'   ##function arguments
#'   x$arguments
#'
#'   #resetting to sequential processing
#'   #future::plan(future::sequential)


  #UNIT TEST ----
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
    x <- collinear(),
    regexp = "'df' cannot be NULL"
  )

  #DF ONLY ----

  ##fewer than 10 rows ----
  testthat::expect_warning(
    x <- collinear(
      df = vi_smol[1:9, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 10 rows"
  ) |>
    suppressMessages()

  ##fewer than 30 rows ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol[1:11, ],
      predictors = vi_predictors_numeric
    ),
    regexp = "has fewer than 30 rows"
  ) |>
    suppressMessages()

  ##more than 30 rows ----

  #max_cor and max_vif NULL
  testthat::expect_error(
    x <- collinear(
      df = vi_smol,
      predictors = vi_predictors_numeric,
      max_cor = NULL,
      max_vif = NULL
    )
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
      df = vi_smol[, c(vi_responses[1:2], vi_predictors_numeric)],
      response = vi_responses[1:2],
      encoding_method = NULL,
      preference_order = NULL,
      f = NULL,
      quiet = FALSE
    ),
    regexp = "ranking predictors from lower to higher multicollinearity"
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
      response = NULL,
      predictors = vi_predictors_numeric,
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
      df = vi,
      response = NULL,
      predictors = vi_predictors_categorical[1:5],
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
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors,
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
    df = vi,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    quiet = TRUE
  )

  testthat::expect_true(
    inherits(x = x, what = "collinear_output")
  )

  testthat::expect_true(
    all(c("linear", "smooth") %in% names(x$vi_numeric$formulas))
  )


  ##categorical numeric ----
  x <- collinear(
    df = vi,
    response = "vi_categorical",
    predictors = vi_predictors_numeric,
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
      df = vi_smol,
      response = "vi_categorical",
      predictors = vi_predictors_categorical,
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
      df = vi_smol,
      response = vi_responses,
      predictors = vi_predictors,
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
    all(vi_responses %in% names(x))
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
    all(selections$Freq == length(vi_responses))
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
      df = vi_smol,
      response = c("vi_numeric", "vi_categorical"),
      predictors = vi_predictors_categorical,
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
    !is.numeric(vi[["soil_type"]]) &&
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
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_order,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "character vector 'preference_order' does not contain any column names in 'df' and  will be ignored"
  ) |>
    suppressMessages()

  testthat::expect_true(
    !all(preference_order %in% x$result$selection)
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
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = preference_order,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all(preference_order %in% x$vi_numeric$selection)
  )

  testthat::expect_true(
    all(preference_order %in% x$arguments$preference_order)
  )

  ### valid dataframe ----
  preference_df <- preference_order(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    f = f_r2_pearson,
    quiet = TRUE
  )

  x <- collinear(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = preference_df,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all.equal(
    target = x$arguments$preference_order,
    current = x$vi_numeric$preference$df
    )
  )

  testthat::expect_true(
    x$arguments$preference_order$f[1] == "f_r2_pearson"
  )

  testthat::expect_true(
    x$vi_numeric$preference$f$name == "f_r2_pearson"
  )

  ### invalid data frame ----
  preference_df$response <- "hola"
  preference_df$f <- NULL

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_df,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "does not match the values in argument 'response'"
  ) |>
    suppressMessages()

  testthat::expect_null(
    x$arguments$preference_order
  )

  ### valid list ----
  preference_list <- preference_order(
    df = vi_smol,
    response = c("vi_numeric", "vi_binomial"),
    predictors = vi_predictors_numeric,
    f = f_r2_pearson,
    quiet = TRUE
  )

  x <- collinear(
    df = vi_smol,
    response = c("vi_numeric", "vi_binomial"),
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = preference_list,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all(
      c("vi_numeric", "vi_binomial") %in% names(x$arguments$preference_order)
    )
  )

  testthat::expect_true(
    x$arguments$preference_order$vi_numeric$f[1] == preference_list$vi_numeric$f[1]
  )

  testthat::expect_true(
    x$arguments$preference_order$vi_binomial$f[1] == preference_list$vi_binomial$f[1]
  )

  #invalid list
  names(preference_list) <- c("a", "b")

  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
      encoding_method = NULL,
      preference_order = preference_list,
      f = NULL,
      max_cor = 0.75,
      max_vif = 5,
      quiet = FALSE
    ),
    regexp = "list 'preference_order' does not contain any element named after the values in 'response'"
  ) |>
    suppressMessages()

  testthat::expect_null(
    x$arguments$preference_order
  )

  testthat::expect_null(
    x$vi_numeric$preference
  )

  ### NULL response ----
  #same as above!
  preference <- c(
    "soil_soc",
    "growing_season_temperature"
  )

  x <- collinear(
    df = vi_smol,
    response = NULL,
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = preference,
    f = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    all(preference %in% x$result$selection)
  )

  testthat::expect_true(
    all(preference %in% x$arguments$preference_order)
  )

  ### f_auto ----
  testthat::expect_message(
    x <- collinear(
      df = vi_smol,
      response = "vi_numeric",
      predictors = vi_predictors_numeric,
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
    x$vi_numeric$selection[1] == x$vi_numeric$preference$df$predictor[1]
  )

  testthat::expect_true(
    x$arguments$f_name == "f_auto"
  )

  testthat::expect_true(
    x$vi_numeric$preference$df$f[1] == "f_r2_pearson"
  )

  ### f_r2_rf ----
  x <- collinear(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    encoding_method = NULL,
    preference_order = NULL,
    f = f_r2_rf,
    max_cor = 0.75,
    max_vif = 5,
    quiet = TRUE
  )

  testthat::expect_true(
    x$vi_numeric$selection[1] == x$vi_numeric$preference$df$predictor[1]
  )

  testthat::expect_true(
    !is.null(x$arguments$f)
  )

  testthat::expect_true(
    x$arguments$f_name == "f_r2_rf"
  )

  ### bad function name ----
  my_f <- function(){return(NULL)}

  testthat::expect_error(
    x <- collinear(
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
      encoding_method = NULL,
      preference_order = NULL,
      f = my_f,
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "to receive a data frame with the column names"
  )

  ### character function name ----
  testthat::expect_error(
    x <- collinear(
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
      encoding_method = NULL,
      preference_order = NULL,
      f = "my_f",
      max_cor = 0.75,
      max_vif = 5,
      quiet = TRUE
    ),
    regexp = "must be a uquoted function name"
  )

  ###wrong function type ----
  testthat::expect_error(
    x <- collinear(
      df = vi,
      response = "vi_numeric",
      predictors = vi_predictors,
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
