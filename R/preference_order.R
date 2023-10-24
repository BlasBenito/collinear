#' Compute the preference order for predictors based on a user-defined function.
#'
#' This function calculates the preference order of predictors based on a user-provided function that takes a predictor, a response, and a data frame as arguments.
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (required, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param f (optional: function) A function that returns a value representing the relationship between a given predictor and the response. Higher values are ranked higher. The available options are:
#' \itemize{
#'  \item f_rsquared (default option): returns the R-squared of the correlation between the response and the predictor.
#'  \item f_lm_coef: returns the absolute coefficient of a linear model of the response against the scaled predictor.
#'  \item f_gam_deviance: returns the explained deviance of a GAM model of the response against a predictor. Requires the package `mgcv`.
#'  \item f_f_variance: returns the explained deviance of a random forest model of the response against the predictor. Requires the package `ranger`.
#' }
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#' @param workers (integer) number of workers for parallel execution. Default: 1
#'
#' @return A data frame with the columns "predictor" and "value". The former contains the predictors names in order, ready for the argument `preference_order` in [cor_select()], [vif_select()] and [collinear()]. The latter contains the result of the function `f` for each combination of predictor and response.
#' @examples
#'
#'data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #computing preference order
#' #with response
#' #numeric and categorical predictors in the output
#' #as the R-squared between each predictor and the response
#' preference.order <- preference_order(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   f = f_rsquared,
#'   workers = 1
#'   )
#'
#' preference.order
#'
#' #using it in variable selection with collinear()
#' selected.predictors <- cor_select(
#'   df = vi,
#'   response = "vi_mean", #don't forget the response!
#'   predictors = vi_predictors,
#'   preference_order = preference.order,
#'   max_cor = 0.75
#'   )
#'
#' selected.predictors
#'
#' #check their correlations
#' selected.predictors.cor <- cor_df(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = selected.predictors
#' )
#'
#' #all correlations below max_cor
#' selected.predictors.cor
#'
#' #USING A CUSTOM FUNCTION
#' #custom function to compute RMSE between a predictor and a response
#' #x is a predictor name
#' #y is a response name
#' #df is a data frame with multiple predictors and one response
#' #must return a single number, with higher number indicating higher preference
#' #notice we use "one minus RMSE" to give higher rank to variables with lower RMSE
#' f_rmse <- function(x, y, df){
#'
#'   xy <- df[, c(x, y)] |>
#'     na.omit() |>
#'     scale()
#'
#'   1 - sqrt(mean((xy[, 1] - xy[, 2])^2))
#'
#' }
#'
#' preference.order <- preference_order(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   f = f_rmse,
#'   workers = 1
#' )
#'
#' preference.order
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
preference_order <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    f = f_rsquared,
    encoding_method = "mean",
    workers = 1
){

  #check workers
  workers <- as.integer(workers)
  if(workers < 1){
    workers <- 1
  }

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- validate_response(
    df = df,
    response = response
  )

  #stop if NULL
  if(is.null(response)){
    stop("the argument 'response' must be the name of a numeric variable in 'df', but it is NULL instead.")
  }

  #target encode character predictors
  df <- target_encoding_lab(
    df = df,
    response = response,
    predictors = predictors,
    encoding_methods = encoding_method,
    replace = TRUE,
    verbose = FALSE
  )

  #get numeric predictors only
  predictors <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  #computing preference order
  preference <- data.frame(
    predictor = predictors
  )

  #default lapply function
  lapply_custom <- lapply

  #run preference order
  if(workers > 1){

    if(
      requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)
    ){

      future::plan(
        strategy = future::multisession,
        workers = workers
      )

      lapply_custom <- future.apply::future_lapply

    } else {

      message("R packages 'future' and 'future.apply' are required to run this function in parallel. but are not installed. Running preference_order() sequentially.")

    }

  }

  #computing preference order
  preference$preference <- lapply_custom(
    X = preference$predictor,
    FUN = function(x){
      f(
        x = x,
        y = response,
        df = df
      )
    }
  ) |>
    unlist() |>
    suppressWarnings()

  #reorder preference
  preference <- preference |>
    dplyr::arrange(
      dplyr::desc(preference)
    )

  preference

}

#' R-squared between a response and a predictor
#'
#' Computes the R-squared between a response and a predictor.
#' Fastest option to compute preference order.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' f_rsquared(
#'   x = "growing_season_length", #predictor
#'   y = "vi_mean",               #response
#'   df = vi
#' )
#'
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

#' Explained Deviance from univariate GAM model
#'
#' Computes the explained deviance of a response against a predictor via Generalized Additive Model (GAM). This option is slower than [f_rsquared()], but suitable if you will be fitting GAMs with the resulting preference order.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Explained deviance
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed in the system
#' if(requireNamespace(package = "mgcv", quietly = TRUE)){
#'
#'   f_gam_deviance(
#'     x = "growing_season_length", #predictor
#'     y = "vi_mean",               #response
#'     df = vi
#'   )
#'
#' }
#'
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

    stop("the function 'f_gam_deviance()' requires the package 'mgcv'.")

  }

}

#' R-squared of Random Forest model from out-of-bag data
#'
#' Computes a univariate random forest model with `\link[ranger]{ranger}` and returns the R-squared on the out-of-bag data.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "ranger" installed in the system
#' if(requireNamespace(package = "ranger", quietly = TRUE)){
#'
#'   f_rf_deviance(
#'     x = "growing_season_length", #predictor
#'     y = "vi_mean",               #response
#'     df = vi
#'   )
#'
#' }
#'
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

    stop("The function 'f_fr_deviance()' requires the package 'ranger'.")

  }

}
