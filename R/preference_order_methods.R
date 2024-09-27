#' Pearson's R-squared of Observations vs Predictions
#'
#' @description
#' Internal function to compute the R-squared of observations versus model predictions.
#'
#'
#' @param o (required, numeric vector) Observations, values of a response variable. Default: NULL
#' @param p (required, numeric vector) Model predictions. Default: NULL
#'
#' @return numeric: Pearson R-squared
#' @export
r2 <- function(
    o = NULL,
    p = NULL
){

  stats::cor(
    x = p,
    y = o,
    method = "pearson",
    use = "complete.obs"
  )^2

}

#' Area Under the Curve of Binomial Observations vs Probabilistic Model Predictions
#'
#' @description
#' Internal function to compute the AUC of binomial models within [preference_order()]. As it is build for speed, this function does not check the inputs.
#'
#'
#' @param o (required, binomial vector) Observations, values of a binomial response variable with unique values 0 and 1. Default: NULL
#' @param p (required, numeric vector) Continuous predictions of a binomial model in the range 0-1. Default: NULL
#'
#' @return numeric: Area Under the Curve
#' @export
#' @family preference_order
#' @autoglobal
auc <- function(
    o = NULL,
    p = NULL
){

  #predicted values of the ones and the zeroes
  ones <- p[o == 1]
  zeros <- p[o == 0]

  #lengths of each vector
  ones.n <- length(ones)
  zeros.n <- length(zeros)

  #curve computation
  curve <- sum(
    rank(c(ones, zeros))[1:n.ones]
    ) - (n.ones * (n.ones + 1) / 2)

  #area under the curve
  curve / (zeros.n * ones.n)

}


#' Association Between Continuous Responses and Predictors
#'
#' @description
#' These functions take a data frame with two numeric columns "x" (predictor) and "y" (response), where "y" can either be continuous or integer counts.  assess their associations directly, or fit a model and assess the R-squared of the observations versus the model predictions:
#' \itemize{
#'   \item `f_r2_pearson()`: Pearson's R-squared between a response and a predictor.
#'   \item `f_r2_spearman()`: Spearman's R-squared between a response and a predictor.
#'   \item `f_r2_gam_gaussian()` Pearson's R-squared between a continuous response and the predictions of a [mgcv::gam()] model with formula `y ~ s(x)` and family `stats::gaussian(link = "identity")`.
#'   \item `f_r2_gam_poisson()` Pearson's R-squared between a count response and the predictions of a [mgcv::gam()] model with formula `y ~ s(x)` and family `stats::poisson(link = "log")`.
#'   \item `f_r2_glm_gaussian_poly2()`: Pearson's R-squared between and a response and the predictions of a GLM model with formula `y ~ stats::poly(x, degree = 2, raw = TRUE)` and family `stats::gaussian(link = "identity")`.
#'   \item `f_r2_glm_poisson_poly2()` Pearson's R-squared between a count response and the predictions of a GLM model with formula `y ~ stats::poly(x, degree = 2, raw = TRUE)` and family `stats::poisson(link = "log")`.
#'   \item `f_r2_rpart()`: Pearson's R-squared between a continuous or count response and the predictions of a [rpart::rpart()] (recursive partition trees) model.
#'   \item `f_r2_rf()`: Pearson's R-squared between a continuous or count response and the predictions of a random forest model fitted with [ranger::ranger()]
#' }
#'
#' @param df (required, data frame) data frame with the numeric column 'x' with a continuous predictor and a continuous (including integer counts) response.
#'
#' @return numeric: R-squared
#' @examples
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #numeric response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_mean"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' # Continuous response
#'
#' #Pearson R-squared
#' f_r2_pearson(df = df)
#'
#' #Spearman R-squared
#' f_r2_spearman(df = df)
#'
#' #R-squared of a gaussian gam
#' f_r2_gam_gaussian(df = df)
#'
#' #gaussian glm with second-degree polynomials
#' f_r2_glm_gaussian_poly2(df = df)
#'
#' #recursive partition tree
#' f_r2_rpart(df = df)
#'
#' #random forest model
#' f_r2_rf(df = df)
#'
#' #Count response
#'
#' #simulating counts in df
#' df$y <- as.integer(df$y * 1000)
#'
#' #GLM model with second degree polynomials an Poisson family
#' f_r2_glm_poisson_poly2(df = df)
#'
#' #GAM model with Poisson family
#' f_r2_gam_poisson(df = df)
#'
#' #tree models manage counts without issues too
#' f_r2_rpart(df = df)
#' f_r2_rf(df = df)
#'
#'
#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_pearson <- function(df){

  stats::cor(
    x = df[["x"]],
    y = df[["y"]],
    method = "pearson"
  )^2

}


#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_spearman <- function(df){

  stats::cor(
    x = df[["x"]],
    y = df[["y"]],
    method = "spearman"
  )^2

}

#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_gam_gaussian <- function(df){

  p <- mgcv::gam(
    formula = y ~ s(x),
    data = df,
    family = stats::gaussian(link = "identity")
  ) |>
    stats::predict(
      type = "response"
    )

  r2(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_gam_poisson <- function(df){

  p <- mgcv::gam(
    formula = y ~ s(x),
    data = df,
    family = stats::poisson(link = "log")
  ) |>
    stats::predict(
      type = "response"
    )

  r2(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_glm_gaussian_poly2 <- function(df){

  p <- stats::glm(
    formula = y ~ stats::poly(
      x,
      degree = 2,
      raw = TRUE
      ),
    data = df,
    family = stats::gaussian(
      link = "identity"
      )
  ) |>
    stats::predict(
      type = "response"
    ) |>
    suppressWarnings() |>
    suppressMessages()

  r2(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_glm_poisson_poly2 <- function(df){

  p <- stats::glm(
    formula = y ~ stats::poly(
      x,
      degree = 2,
      raw = TRUE
    ),
    data = df,
    family = stats::poisson(
      link = "log"
      )
  ) |>
    stats::predict(
      type = "response"
    ) |>
    suppressWarnings() |>
    suppressMessages()

  r2(
    o = df[["y"]],
    p = p
  )

}


#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_rpart <- function(df){

  p <- rpart::rpart(
    formula = y ~ x,
    data = df,
    control = rpart::rpart.control(
      minbucket = ceiling(
        nrow(df)/100
      )
    )
  ) |>
    stats::predict(
      type = "vector"
    )

  r2(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @export
f_r2_rf <- function(df){

  m <- ranger::ranger(
    formula = y ~ x,
    data = df,
    num.threads = 1,
    min.node.size = ceiling(nrow(df)/100),
    seed = 1
  )

  p <-stats::predict(
    object = m,
    data = df
    )$predictions

  r2(
    o = df[["y"]],
    p = p
  )

}

#' Cramer's V Between Discrete Responses and Predictors
#'
#' @description
#' Please see [cramer_v()] for details.
#'
#'
#' @inheritParams f_r2_pearson
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


#' R-squared From a Univariate Poisson GAM Model
#'
#' Computes the R-squared of a univariate Generalized Additive Model (GAM) fitted with the formula `formula = y ~ s(x)` and the family `stats::poisson(link = "log")`.
#'
#' @inheritParams f_r2_pearson
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
#' @inheritParams f_r2_pearson
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
    p = stats::predict(m, type = "response")
  )

}


#' AUC of Logistic GAM Model for Unbalanced Binary Responses
#'
#' Fits a quasi-binomial logistic Generalized Additive Model (GAM) `y ~ s(x, k = 3)` with weighted cases between a binary response and a numeric predictor and returns the Area Under the Curve of the observations versus the predictions.
#'
#'
#' @inheritParams f_r2_pearson
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




#' AUC of Random Forest Model for Unbalanced Binary Responses
#'
#' Computes a univariate random forest model with weighted cases via `\link[ranger]{ranger}` and returns the Area Under the Curve on the out-of-bag data.
#'
#' @inheritParams f_r2_pearson
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
#' @inheritParams f_r2_pearson
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


#' AUC of Binomial GLM with Logit Link for Balance Binary Responses
#'
#' Fits a logistic GLM model `y ~ x` when `y` is a binary response with values 0 and 1 and `x` is numeric. This function is suitable when the response variable is balanced. If the response is unbalanced, then [f_logistic_auc_unbalanced()] should provide better results.
#'
#'
#' @inheritParams f_r2_pearson
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
#' @inheritParams f_r2_pearson
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
