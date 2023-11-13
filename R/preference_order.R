#' Compute the preference order for predictors based on a user-defined function.
#'
#' This function calculates the preference order of predictors based on a user-provided function that takes a predictor, a response, and a data frame as arguments.
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (required, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param f (optional: function) A function that returns a value representing the relationship between a given predictor and the response. Higher values are ranked higher. The available options are:
#' \itemize{
#'  \item [f_rsquared()] (default option): returns the R-squared of the correlation between a numeric response and a numeric predictor.
#'  \item f_gam_deviance: fits a univariate GAM model between a numeric response and a numeric predictor to return the explained deviance. Requires the package `mgcv`.
#'  \item [f_rf_rsquared()] also named [f_rf_deviance()]: fits a univariate random forest model with `ranger::ranger()` between a numeric response and a numeric predictor to return the R-squared of the observations against the out-of-bag predictions. Requires the package `ranger`.
#'  \item [f_logistic_auc_balanced()]: fits a logistic univariate GLM of a balanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
#'  \item [f_logistic_auc_unbalanced()]: fits a quasibinomial univariate GLM with weighted cases of an unbalanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
#'  \item [f_gam_auc_balanced()]: fits a logistic univariate GAM of a balanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
#'  \item [f_gam_auc_unbalanced()]: fits a quasibinomial univariate GAM with weighted cases of an unbalanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
#'  \item [f_rf_auc_balanced()]: fits a random forest model of a balanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
#'  \item [f_rf_auc_unbalanced()]: fits a random forest model with weighted cases of an unbalanced binary response (0s and 1s) against a numeric predictor to return the Area Under the Curve of the observations against the predictors.
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

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("the function 'f_gam_auc_unbalanced()' requires the package 'mgcv'. Please, install it first.")
  }

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

}


#' R-squared of Random Forest model
#'
#' Computes a univariate random forest model with `\link[ranger]{ranger}` and returns the R-squared on the out-of-bag data.
#'
#' `f_rf_rsquared()` and `f_rf_deviance()` are synonyms
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
#'   f_rf_rsquared(
#'     x = "growing_season_length", #predictor
#'     y = "vi_mean",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
f_rf_rsquared <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("the function 'f_fr_deviance()' requires the package 'ranger'. Please install it first.")
  }

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

}

#' @rdname f_rf_rsquared
#' @export
f_rf_deviance <- f_rf_rsquared


#' AUC of Random Forest model of an unbalanced binary response
#'
#' Computes a univariate random forest model with weighted cases via `\link[ranger]{ranger}` and returns the Area Under the Curve on the out-of-bag data.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
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
#'   f_rf_auc_unbalanced(
#'     x = "growing_season_length", #predictor
#'     y = "vi_binary",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
f_rf_auc_unbalanced <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("the function 'f_fr_deviance()' requires the package 'ranger'. Please install it first.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

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
    case.weights = case_weights(x = data$y),
    seed = 1
  )

  auc_score(
    observed = data$y,
    predicted = m$predictions
  )

}



#' AUC of Random Forest model of a balanced binary response
#'
#' Computes a univariate random forest model  cases via `\link[ranger]{ranger}` and returns the Area Under the Curve on the out-of-bag data.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
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
#'   f_rf_auc_balanced(
#'     x = "growing_season_length", #predictor
#'     y = "vi_binary",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
f_rf_auc_balanced <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("the function 'f_fr_deviance()' requires the package 'ranger'. Please install it first.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

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

  auc_score(
    observed = data$y,
    predicted = m$predictions
  )

}


#' AUC of Binomial GLM with Logit Link
#'
#' Fits a logistic GLM model `y ~ x` when `y` is a binary response with values 0 and 1 and `x` is numeric. This function is suitable when the response variable is balanced. If the response is unbalanced, then [f_logistic_auc_unbalanced()] should provide better results.
#'
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' f_logistic_auc_balanced(
#'   x = "growing_season_length", #predictor
#'   y = "vi_binary",             #binary response
#'   df = vi
#' )
#'
#' @autoglobal
#' @export
f_logistic_auc_balanced <- function(x, y, df){

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- stats::glm(
    formula = y ~ x,
    data = data,
    family = stats::binomial(link = "logit")
  ) |>
    suppressWarnings()

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}


#' AUC of Binomial GLM with Logit Link and Case Weights
#'
#' Fits a quasibinomial GLM model `y ~ x` with case weights when `y` is an unbalanced binary response with values 0 and 1 and `x` is numeric. It uses the function [case_weights()] to weight 0s and 1s according to their frequency within `y`.
#'
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable.
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' f_logistic_auc_unbalanced(
#'   x = "growing_season_length", #predictor
#'   y = "vi_binary",             #binary response
#'   df = vi
#' )
#'
#' @autoglobal
#' @export
f_logistic_auc_unbalanced <- function(x, y, df){

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- stats::glm(
    formula = y ~ x,
    data = data,
    family = stats::quasibinomial(link = "logit"),
    weights = case_weights(x = data$y)
  ) |>
    suppressWarnings()

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}


#' AUC of Logistic GAM Model
#'
#' Fits a binomial logistic Generalized Additive Model (GAM) `y ~ s(x, k = 3)` between a binary response and a numeric predictor and returns the Area Under the Curve of the observations versus the predictions.
#'
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable.
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed
#' if(requireNamespace(package = "mgcv", quietly = TRUE)){
#'
#'   f_gam_auc_balanced(
#'     x = "growing_season_length", #predictor
#'     y = "vi_binary",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
f_gam_auc_balanced <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("the function 'f_gam_auc_unbalanced()' requires the package 'mgcv'. Please, install it first.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- mgcv::gam(
    formula = y ~ s(x, k = 3),
    data = data,
    family = stats::binomial(link = "logit")
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}


#' AUC of Logistic GAM Model with Weighted Cases
#'
#' Fits a quasibinomial logistic Generalized Additive Model (GAM) `y ~ s(x, k = 3)` with weighted cases between a binary response and a numeric predictor and returns the Area Under the Curve of the observations versus the predictions.
#'
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the binary response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return Area Under the Curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed
#' if(requireNamespace(package = "mgcv", quietly = TRUE)){
#'
#'   f_gam_auc_unbalanced(
#'     x = "growing_season_length", #predictor
#'     y = "vi_binary",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @export
f_gam_auc_unbalanced <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("the function 'f_gam_auc_unbalanced()' requires the package 'mgcv'. Please, install it first.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with 0s and 1s")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- mgcv::gam(
    formula = y ~ s(x, k = 3),
    data = data,
    family = stats::quasibinomial(link = "logit"),
    weights = case_weights(x = data$y)
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}


#' @title Area Under the Receiver Operating Characteristic
#' @description Computes the AUC score of binary model predictions.
#' @param observed (required, integer) Numeric vector with observations. Valid values are 1 and 0. Must have the same length as `predicted`. Default: NULL
#' @param predicted (required, numeric) Numeric vector in the range 0-1 with binary model predictions. Must have the same length as `observed`.
#' @return AUC value.
#' @examples
#'
#'  out <- auc_score(
#'    observed = c(0, 0, 1, 1),
#'    predicted = c(0.1, 0.6, 0.4, 0.8)
#'    )
#'
#' @export
auc_score <- function(
    observed = NULL,
    predicted = NULL
){

  if(is.null(observed) | is.null(predicted)){
    stop("The arguments 'observed' and 'predicted' must not be NULL.")
  }

  #check observations
  if(all(sort(unique(observed)) == c(0, 1)) == FALSE){
    stop("Argument 'observed' must be a integer vector with 0s and 1s")
  }

  #check predictions
  if(min(predicted) < 0 | max(predicted) > 1){
    stop("Argument 'predicted' must be a numeric vector with values between 0 and 1.")
  }

  #predicted values of the ones and the zeroes
  ones <- stats::na.omit(predicted[observed == 1])
  zeros <- stats::na.omit(predicted[observed == 0])

  #lengths of each vector
  n.ones <- length(ones)
  n.zeros <- length(zeros)

  #curve computation
  curve <- sum(
    rank(c(ones, zeros))[1:n.ones]) -
    (n.ones*(n.ones+1)/2
    )

  #area under the curve
  auc <- curve / (n.zeros * n.ones)

  auc

}


#' @title Case Weights for Unbalanced Binary Response
#' @param x (required, integer vector) Binary response with 1s and 0s. Default: `NULL`
#' @return A vector with a length equal to `x` with case weights.
#' @examples
#' if(interactive()){
#'
#'  case_weights(
#'    x = c(0, 0, 0, 1, 1)
#'  )
#'
#'  }
#' @rdname internal
#' @keywords internal
#' @export
case_weights <- function(
    x = NULL
){

  if(is.null(x)){
    stop("Argument 'x' must not be NULL.")
  }

  #check that x is binary
  if(all(sort(unique(x)) == c(0, 1)) == FALSE){
    stop("Argument 'x' must be a integer vector with 0s and 1s")
  }

  #counting number of ones and zeros
  n <- length(x)
  n.1 <- sum(x)
  n.0 <- n - n.1

  #computing weights
  weight.1 <- 1/n.1
  weight.0 <- 1/n.0

  #vector of weights
  case.weights <- rep(NA, n)
  case.weights[x == 1] <- weight.1
  case.weights[x == 0] <- weight.0

  case.weights

}
