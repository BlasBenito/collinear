# continuous response and predictor ----



#' Relationship Between a Continuous Response and a Continuous Predictor
#'
#' @description
#' R-squared between a numeric response `y` and a numeric predictor `x`.
#'
#' @param x (required, character string) name of the predictor variable.
#' @param y (required, character string) name of the response variable
#' @param df (required, data frame) data frame with the columns 'x' and 'y'.
#'
#' @return numeric: R-squared
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
#' @family preference_order
#' @export
f_rsquared <- function(x, y, df){

  stats::cor(
    x = df[[x]],
    y = df[[y]],
    use = "pairwise.complete.obs",
    method = "pearson"
  )^2

}


#' Area Under the Curve Between Binary Responses and Predictors
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: Cramer's V
#' @examples
#' TODO complete this example
#'
#' @autoglobal
#' @family preference_order
#' @export
f_auc <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  auc_score(
    observed = data$y,
    predicted = data$x
  )

}


#' Cramer's V Between Discrete Responses and Predictors
#'
#' @inheritSection cramer_v Description
#' @inheritParams f_rsquared
#'
#' @return numeric: Cramer's V
#' @examples
#' TODO complete this example
#'
#' @autoglobal
#' @family preference_order
#' @export
f_cramer_v <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  if(
    any(
      sapply(
        X = data,
        FUN = is.numeric
      )
    )
  ){
    stop("f_cramer_v: arguments 'x' and 'y' must be names of character or factor columns in 'df'.")
  }

  cramer_v(
    x = data$x,
    y = data$y,
    check_input = FALSE
  )

}


#' R-squared From a Univariate Gaussian GAM Model
#'
#' Computes the R-squared of a univariate Generalized Additive Model (GAM) fitted with the formula `formula = y ~ s(x)` and the family `stats::gaussian(link = "identity")`.
#'
#' @inheritParams f_rsquared
#'
#' @return R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed in the system
#' if(
#'   requireNamespace(
#'     package = "mgcv",
#'    quietly = TRUE
#'    )
#'  ){
#'
#'   f_gam_gaussian_rsquared(
#'     x = "growing_season_length", #predictor
#'     y = "vi_mean",               #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @family preference_order
#' @export
f_gam_gaussian_rsquared <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("The function 'f_gam_rsquared()' requires the package 'mgcv'.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- mgcv::gam(
    formula = y ~ s(x),
    data = data,
    family = stats::gaussian(link = "identity")
  )

  stats::cor(
    x = data$y,
    y = stats::predict(
      object = m,
      type = "response"
      )
  )^2

}


#' R-squared From a Univariate Poisson GAM Model
#'
#' Computes the R-squared of a univariate Generalized Additive Model (GAM) fitted with the formula `formula = y ~ s(x)` and the family `stats::poisson(link = "log")`.
#'
#' @inheritParams f_rsquared
#'
#' @return R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed in the system
#' if(
#'   requireNamespace(
#'     package = "mgcv",
#'    quietly = TRUE
#'    )
#'  ){
#'
#'  #simulate counts in vi$vi_mean
#'  vi$vi_mean_count <- as.integer(vi$vi_mean * 1000)
#'
#'   f_gam_poisson_rsquared(
#'     x = "growing_season_length", #predictor
#'     y = "vi_mean_count",         #response
#'     df = vi
#'   )
#'
#' }
#'
#' @autoglobal
#' @family preference_order
#' @export
f_gam_poisson_rsquared <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("The function 'f_gam_rsquared()' requires the package 'mgcv'.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- mgcv::gam(
    formula = y ~ s(x),
    data = data,
    family = stats::poisson(link = "log")
  ) |>
    suppressWarnings()

  stats::cor(
    x = data$y,
    y = stats::predict(
      object = m,
      type = "response"
      )
  )^2

}

#' AUC of Logistic GAM Model for Balanced Binary Responses
#'
#' Fits a binomial logistic Generalized Additive Model (GAM) `y ~ s(x, k = 3)` between a binary response and a numeric predictor and returns the Area Under the Curve of the observations versus the predictions.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed
#' if(
#'   requireNamespace(
#'     package = "mgcv",
#'     quietly = TRUE
#'     )
#'  ){
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
#' @family preference_order
#' @export
f_gam_binomial_balanced_auc <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("The function 'f_gam_auc_balanced()' requires the package 'mgcv'.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
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


#' AUC of Logistic GAM Model for Unbalanced Binary Responses
#'
#' Fits a quasi-binomial logistic Generalized Additive Model (GAM) `y ~ s(x, k = 3)` with weighted cases between a binary response and a numeric predictor and returns the Area Under the Curve of the observations versus the predictions.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "mgcv" installed
#' if(
#'   requireNamespace(
#'     package = "mgcv",
#'     quietly = TRUE
#'     )
#'   ){
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
#' @family preference_order
#' @export
f_gam_binomial_unbalanced_auc <- function(x, y, df){

  if(!requireNamespace("mgcv", quietly = TRUE)){
    stop("The function 'f_gam_auc_unbalanced()' requires the package 'mgcv'.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- mgcv::gam(
    formula = y ~ s(x),
    data = data,
    family = stats::quasibinomial(link = "logit"),
    weights = case_weights(x = data$y)
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(
      object = m,
      type = "response"
    )
  )

}


#' R-squared of Random Forest model
#'
#' Computes a univariate random forest model with `\link[ranger]{ranger}` and returns the R-squared on the out-of-bag data.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "ranger" installed in the system
#' if(
#' requireNamespace(
#'   package = "ranger",
#'   quietly = TRUE
#'   )
#' ){
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
#' @family preference_order
#' @export
f_rf_rsquared <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("The function 'f_rf_rsquared()' requires the package 'ranger'.")
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

  stats::cor(
    x = data$y,
    y = stats::predict(
      object = m,
      data = data
    )$predictions
  )^2

}


#' R-squared of Recursive Partition model
#'
#' Computes a univariate random forest model with `\link[ranger]{ranger}` and returns the R-squared on the out-of-bag data.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: R-squared
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "ranger" installed in the system
#' if(
#' requireNamespace(
#'   package = "ranger",
#'   quietly = TRUE
#'   )
#' ){
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
#' @family preference_order
#' @export
f_rpart_rsquared <- function(x, y, df){

  if(!requireNamespace("rpart", quietly = TRUE)){
    stop("The function 'f_rpart_rsquared()' requires the package 'rpart'.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- rpart::rpart(
    formula = y ~ x,
    data = data,
    control = rpart::rpart.control(
      minbucket = ceiling(
        nrow(data)/100
      )
    )
  )

  stats::cor(
    x = data$y,
    y = stats::predict(
      object = m,
      type = "vector"
    )
  )^2

}


#' AUC of Random Forest Model for Unbalanced Binary Responses
#'
#' Computes a univariate random forest model with weighted cases via `\link[ranger]{ranger}` and returns the Area Under the Curve on the out-of-bag data.
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
#' @examples
#'
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #this example requires "ranger" installed in the system
#' if(
#'   requireNamespace(
#'     package = "ranger",
#'     quietly = TRUE
#'     )
#'   ){
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
#' @family preference_order
#' @export
f_rf_binomial_unbalanced_auc <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("The function 'f_rf_auc_unbalanced()' requires the package 'ranger'.")
  }

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
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
    predicted = stats::predict(
      object = m,
      data = data
    )$predictions
  )

}



#' AUC of Random Forest Model for Balanced Binary Responses
#'
#' Computes a univariate random forest model via [ranger::ranger()] and returns the Area Under the Curve on the out-of-bag data.
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
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
#' @family preference_order
#' @export
f_rf_binomial_balanced_auc <- function(x, y, df){

  if(!requireNamespace("ranger", quietly = TRUE)){
    stop("The function 'f_rf_auc_balanced()' requires the package 'ranger'")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  if(all(sort(unique(data$y)) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  m <- ranger::ranger(
    formula = y ~ x,
    data = data,
    num.threads = 1,
    min.node.size = ceiling(nrow(data)/100),
    seed = 1
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(
      object = m,
      data = data
    )$predictions
  )

}


f_rpart_binomial_unbalanced_auc <- function(x, y, df){

  if(!requireNamespace("rpart", quietly = TRUE)){
    stop("The function 'f_rpart_rsquared()' requires the package 'rpart'.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  if(all(sort(unique(data$y)) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  m <- rpart::rpart(
    formula = y ~ x,
    data = data,
    weights = case_weights(x = data$y),
    control = rpart::rpart.control(
      minbucket = ceiling(
        nrow(data)/100
      )
    )
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(
      object = m,
      type = "vector"
    )
  )

}


f_rpart_binomial_balanced_auc <- function(x, y, df){

  if(!requireNamespace("rpart", quietly = TRUE)){
    stop("The function 'f_rpart_rsquared()' requires the package 'rpart'.")
  }

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  if(all(sort(unique(data$y)) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  m <- rpart::rpart(
    formula = y ~ x,
    data = data,
    control = rpart::rpart.control(
      minbucket = ceiling(
        nrow(data)/100
      )
    )
  )

  auc_score(
    observed = data$y,
    predicted = stats::predict(
      object = m,
      type = "vector"
    )
  )

}


#' R-squared of a Univariate Second-Degree Polynomial GLM
#'
#' Fits a GLM model `y ~ stats::poly(x, degree = 2, raw = TRUE)` with family `stats::gaussian(link = "identity")` and returns its R-squared.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
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
#' @family preference_order
#' @export
f_glm_gaussian_poly2_rsquared <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- stats::glm(
    formula = y ~ stats::poly(x, degree = 2, raw = TRUE),
    data = data,
    family = stats::gaussian(link = "identity")
  ) |>
    suppressWarnings()

  stats::cor(
    x = data$y,
    y = stats::predict(
      object = m,
      type = "response"
    )
  )^2

}

#' AUC of Binomial GLM with Logit Link for Balance Binary Responses
#'
#' Fits a logistic GLM model `y ~ x` when `y` is a binary response with values 0 and 1 and `x` is numeric. This function is suitable when the response variable is balanced. If the response is unbalanced, then [f_logistic_auc_unbalanced()] should provide better results.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
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
#' @family preference_order
#' @export
f_glm_binomial_balanced_auc <- function(x, y, df){

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
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
    predicted = stats::predict(
      object = m,
      type = "response"
      )
  )

}


#' AUC of Binomial GLM with Logit Link for Unbalanced Binary Responses
#'
#' Fits a quasi-binomial GLM model `y ~ x` with case weights when `y` is an unbalanced binary response with values 0 and 1 and `x` is numeric. It uses the function [case_weights()] to weight 0s and 1s according to their frequency within `y`.
#'
#'
#' @inheritParams f_rsquared
#'
#' @return numeric: area under the curve
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
#' @family preference_order
#' @export
f_glm_binomial_unbalanced_auc <- function(x, y, df){

  if(all(sort(unique(df[[y]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
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
    predicted = stats::predict(
      object = m,
      type = "response"
      )
  )

}





#' @title Area Under the Receiver Operating Characteristic
#' @description Computes the AUC score of binary model predictions.
#' @param observed (required, integer) Numeric vector with observations. Valid values are 1 and 0. Must have the same length as `predicted`. Default: NULL
#' @param predicted (required, numeric) Numeric vector in the range 0-1 with binary model predictions. Must have the same length as `observed`.
#' @return numeric: area under the curve
#' @examples
#'
#'  out <- auc_score(
#'    observed = c(0, 0, 1, 1),
#'    predicted = c(0.1, 0.6, 0.4, 0.8)
#'    )
#' @autoglobal
#' @family preference_order
#' @export
auc_score <- function(
    observed = NULL,
    predicted = NULL
){

  if(is.null(observed) | is.null(predicted)){
    stop("Arguments 'observed' and 'predicted' must not be NULL.")
  }

  #check observations
  if(all(sort(unique(observed)) == c(0, 1)) == FALSE){
    stop("Argument 'observed' must be the name of a binary vector with unique values 0 and 1.")
  }

  #check predictions
  if(min(predicted) < 0 | max(predicted) > 1){
    stop("Argument 'predicted' must be the name of a binary vector with unique values 0 and 1.")
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
  curve / (n.zeros * n.ones)

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
#' @family preference_order
#' @autoglobal
#' @export
case_weights <- function(
    x = NULL
){

  if(is.null(x)){
    stop("Argument 'x' must not be NULL.")
  }

  #check that x is binary
  if(all(sort(unique(x)) == c(0, 1)) == FALSE){
    stop("Argument 'x' must be the name of a binary vector with unique values 0 and 1.")
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
