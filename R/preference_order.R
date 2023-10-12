#' Compute the preference order for predictors based on a user-defined function.
#'
#' This function calculates the preference order of predictors based on a user-provided function that takes a predictor, a response, and a data frame as arguments.
#'
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param f (optional: function) A function that returns a value representing the relationship between a given predictor and the response. Higher values are ranked higher. The available options are:
#' \itemize{
#'  \item f_rsquared (default option): returns the R-squared of the correlation between the response and the predictor.
#'  \item f_lm_coef: returns the absolute coefficient of a linear model of the response against the scaled predictor.
#'  \item f_gam_deviance: returns the explained deviance of a GAM model of the response against a predictor. Requires the package `mgcv`.
#'  \item f_f_variance: returns the explained deviance of a random forest model of the response against the predictor. Requires the package `ranger`.
#' }
#' @param workers (integer) number of workers for parallel execution. Default: 1
#'
#' @return A data frame with the columns "predictor" and "value". The former contains the predictors names in order, ready for the argument `preference_order` in `cor_select()`, `vif_select()` and `collinear()`. The latter contains the result of the function `f` for each combination of predictor and response.
#' @autoglobal
#' @export
preference_order <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    f = f_rsquared,
    workers = 1
    ){

  #check input data frame
  df <- df_inspect(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- predictors_inspect(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- response_inspect(
    df = df,
    response = response
  )

  #target encode character predictors
  df <- target_encode(
    df = df,
    response = response,
    predictors = predictors
  )

  #computing preference order
  preference <- data.frame(
    predictor = predictors
  )

  #prepare parallel execution
  if(workers > future::nbrOfFreeWorkers()){
    workers <- future::nbrOfFreeWorkers()
  }

  if(workers > 1){
    future::plan(
      strategy = future::multisession,
      workers = workers
    )
  } else {
    future::plan(
      strategy = future::sequential
    )
  }

  #compute preference
  preference$value <- future.apply::future_lapply(
    X = preference$predictor,
    FUN = function(x){
      f(
        x = x,
        y = response,
        df = df
      )
    }
  ) |>
    unlist()

  #reorder preference
  preference <- preference |>
    dplyr::arrange(
      dplyr::desc(value)
    )

  preference

}

#' Compute the preference order for predictors based on the R-squared value.
#'
#' This function calculates the preference order of predictors based on the R-squared value between each predictor and the response variable.
#'
#' @param x The predictor variable name.
#' @param y The response variable name.
#' @param df The data frame containing the variables.
#'
#' @return The R-squared value between the predictor and the response variable.
#' @autoglobal
#' @export
f_rsquared <- function(x, y, df){

  stats::cor(
    x = df[[x]],
    y = df[[y]],
    use = "pairwise.complete.obs",
    method = "pearson"
  )^2

}

#' Compute the preference order for predictors based on absolute value of the linear regression coefficient.
#'
#' This function calculates the preference order of predictors based on the linear regression coefficient between each predictor and the response variable.
#'
#' @param x The predictor variable name.
#' @param y The response variable name.
#' @param df The data frame containing the variables.
#'
#' @return The absolute linear regression coefficient for the predictor.
#' @autoglobal
#' @export
f_lm_coef <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = scale(df[[x]])
  ) |>
    na.omit()

  m <- stats::lm(
    formula = y ~ x,
    data = data
  )

  abs(stats::coef(m)["x"])

}

#' Compute the preference order for predictors based on the GAM deviance explained.
#'
#' This function calculates the preference order of predictors based on the deviance explained by a Generalized Additive Model (GAM) between each predictor and the response variable.
#'
#' @param x The predictor variable name.
#' @param y The response variable name.
#' @param df The data frame containing the variables.
#'
#' @return The deviance explained by the GAM model for the predictor.
#' @autoglobal
#' @export
f_gam_deviance <- function(x, y, df){

  if(requireNamespace("mgcv", quietly = TRUE)){

    data <- data.frame(
      y = df[[y]],
      x = df[[x]]
    ) |>
      na.omit()

    m <- mgcv::gam(
      formula = y ~ s(x, k = 3),
      data = data
    )

    summary(m)$dev.expl

  } else {

    stop("The function 'gam_deviance()' requires the package 'mgcv'.")

  }

}

#' Compute the preference order for predictors based on the random forest R-squared value.
#'
#' This function calculates the preference order of predictors based on the R-squared value from a Random Forest model between each predictor and the response variable.
#'
#' @param x The predictor variable name.
#' @param y The response variable name.
#' @param df The data frame containing the variables.
#'
#' @return The R-squared value from the Random Forest model for the predictor.
#' @autoglobal
#' @export
f_rf_deviance <- function(x, y, df){

  if(requireNamespace("ranger", quietly = TRUE)){

    data <- data.frame(
      y = df[[y]],
      x = df[[x]]
    ) |>
      na.omit()

    m <- ranger::ranger(
      formula = y ~ x,
      data = data,
      num.threads = 1,
      min.node.size = ceiling(nrow(data)/100),
      seed = 1
    )

    m$r.squared

  } else {

    stop("The function 'gam_deviance()' requires the package 'ranger'.")

  }

}
