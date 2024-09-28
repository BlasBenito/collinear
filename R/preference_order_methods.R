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
    rank(c(ones, zeros))[1:ones.n]
    ) - (ones.n * (ones.n + 1) / 2)

  #area under the curve
  curve / (zeros.n * ones.n)

}


#' Association Between a Continuous Response and a Continuous Predictor
#'
#' @description
#' These functions take a data frame with two numeric continuous columns "x" (predictor) and "y" (response), fit a univariate model, and return the R-squared of the observations versus the model predictions:
#' \itemize{
#'
#'   \item `f_r2_pearson()`: Pearson's R-squared.
#'
#'   \item `f_r2_spearman()`: Spearman's R-squared.
#'
#'   \item `f_r2_glm_gaussian()`: Pearson's R-squared of a GLM model fitted with [stats::glm()], with formula `y ~ s(x)` and family `stats::gaussian(link = "identity")`.
#'
#'   \item `f_r2_glm_gaussian_poly2()`: Pearson's R-squared of a GLM model fitted with [stats::glm()], with formula `y ~ stats::poly(x, degree = 2, raw = TRUE)` and family `stats::gaussian(link = "identity")`.
#'
#'   \item `f_r2_gam_gaussian()`: Pearson's R-squared of a GAM model fitted with [mgcv::gam()], with formula `y ~ s(x)` and family `stats::gaussian(link = "identity")`.

#'   \item `f_r2_rpart()`: Pearson's R-squared of a Recursive Partition Tree fitted with [rpart::rpart()] with formula `y ~ x`.
#'
#'   \item `f_r2_rf()`: Pearson's R-squared of a 100 trees Random Forest model fitted with [ranger::ranger()] and formula `y ~ x`.
#'
#' }
#'
#' @param df (required, data frame) with columns:
#' \itemize{
#'   \item "x": (numeric) continuous predictor.
#'   \item "y" (numeric) continous response.
#' }
#'
#' @return numeric: R-squared
#' @examples
#'
#load example data
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
#' f_r2_glm_gaussian(df = df)
#'
#' #gaussian glm with second-degree polynomials
#' f_r2_glm_gaussian_poly2(df = df)
#'
#' #R-squared of a gaussian gam
#' f_r2_gam_gaussian(df = df)
#'
#' #recursive partition tree
#' f_r2_rpart(df = df)
#'
#' #random forest model
#' f_r2_rf(df = df)
#' @autoglobal
#' @rdname f_r2
#' @family preference_order
#' @examples
#'
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #continuous response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_numeric"]],
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
#' f_r2_glm_gaussian(df = df)
#'
#' #gaussian glm with second-degree polynomials
#' f_r2_glm_gaussian_poly2(df = df)
#'
#' #R-squared of a gaussian gam
#' f_r2_gam_gaussian(df = df)
#'
#' #recursive partition tree
#' f_r2_rpart(df = df)
#'
#' #random forest model
#' f_r2_rf(df = df)
#'
#' @name f_r2
NULL

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
f_r2_glm_gaussian <- function(df){

  p <- stats::glm(
    formula = y ~ x,
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
f_r2_gam_gaussian <- function(df){

  p <- mgcv::gam(
    formula = y ~ s(x),
    data = df,
    family = stats::gaussian(link = "identity"),
    select = TRUE
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
    num.trees = 100,
    min.node.size = ceiling(nrow(df)/100),
    seed = 1
  )

  p <- stats::predict(
    object = m,
    data = df
    )$predictions

  r2(
    o = df[["y"]],
    p = p
  )

}

#' Association Between a Count Response and a Continuous Predictor
#'
#' @description
#' These functions take a data frame with a integer counts response "y", and a continuous predictor "x", fit a univariate model, and return the R-squared of observations versus predictions:
#' \itemize{
#'
#'   \item `f_r2_glm_poisson()` Pearson's R-squared between a count response and the predictions of a GLM model with formula `y ~ x` and family `stats::poisson(link = "log")`.
#'
#'   \item `f_r2_glm_poisson_poly2()` Pearson's R-squared between a count response and the predictions of a GLM model with formula `y ~ stats::poly(x, degree = 2, raw = TRUE)` and family `stats::poisson(link = "log")`.
#'
#'   \item `f_r2_gam_poisson()` Pearson's R-squared between a count response and the predictions of a [mgcv::gam()] model with formula `y ~ s(x)` and family `stats::poisson(link = "log")`.
#'
#'   \item `f_r2_rpart()`: Pearson's R-squared of a Recursive Partition Tree fitted with [rpart::rpart()] with formula `y ~ x`.
#'
#'   \item `f_r2_rf()`: Pearson's R-squared of a 100 trees Random Forest model fitted with [ranger::ranger()] and formula `y ~ x`.
#' }
#'
#' @param df (required, data frame) with columns:
#' \itemize{
#'   \item "x": (numeric) continuous predictor.
#'   \item "y" (integer) counts response.
#' }
#' @rdname f_r2_counts
#' @family preference_order
#' @examples
#'
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #integer counts response and continuous predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_counts"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' #GLM model with Poisson family
#' f_r2_glm_poisson(df = df)
#'
#' #GLM model with second degree polynomials and Poisson family
#' f_r2_glm_poisson_poly2(df = df)
#'
#' #GAM model with Poisson family
#' f_r2_gam_poisson(df = df)
#' @name f_r2_counts
NULL


#' @autoglobal
#' @rdname f_r2_counts
#' @family preference_order
#' @export
f_r2_glm_poisson <- function(df){

  p <- stats::glm(
    formula = y ~ x,
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
#' @rdname f_r2_counts
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
#' @rdname f_r2_counts
#' @family preference_order
#' @export
f_r2_gam_poisson <- function(df){

  p <- mgcv::gam(
    formula = y ~ s(x),
    data = df,
    family = stats::poisson(link = "log"),
    select = TRUE
  ) |>
    stats::predict(
      type = "response"
    )

  r2(
    o = df[["y"]],
    p = p
  )

}

#' Association Between a Binomial Response and a Continuous Predictor
#'
#' @description
#' These functions take a data frame with a binomial response "y" with unique values 1 and 0, and a continuous predictor "x", fit a univariate model, to return the Area Under the ROC Curve (AUC) of observations versus predictions:
#' \itemize{
#'
#'   \item `f_auc_glm_binomial()`: AUC of a binomial response against the predictions of a GLM model with formula `y ~ x`, family `stats::quasibinomial(link = "logit")`, and weighted cases (see [case_weights()]) to control for unbalanced data.
#'
#'   \item `f_auc_glm_binomial_poly2()`: AUC of a binomial response against the predictions of a GLM model with formula `y ~ stats::poly(x, degree = 2, raw = TRUE)`, family `stats::quasibinomial(link = "logit")`, and weighted cases (see [case_weights()]) to control for unbalanced data.
#'
#'   \item `f_auc_gam_binomial()`: AUC  of a GAM model with formula  `y ~ s(x)`, family `stats::quasibinomial(link = "logit")`, and weighted cases.
#'
#'   \item `f_auc_rpart()`: AUC of a Recursive Partition Tree with weighted cases.
#'
#'   \item `f_auc_rf()`: AUC of a Random Forest model with weighted cases.
#' }
#'
#' @param df (required, data frame) with columns:
#' \itemize{
#'   \item "x": (numeric) continuous predictor.
#'   \item "y" (integer) binomial response with unique values 0 and 1.
#' }
#' @family preference_order
#' @examples
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #integer counts response and continuous predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_binomial"]],
#'   x = vi[["swi_max"]]
#' ) |>
#'   na.omit()
#'
#' #AUC of GLM with binomial response and weighted cases
#' f_auc_glm_binomial(df = df)
#'
#' #AUC of GLM as above plus second degree polynomials
#' f_auc_glm_binomial_poly2(df = df)
#'
#' #AUC of binomial GAM with weighted cases
#' f_auc_gam_binomial(df = df)
#'
#' #AUC of recursive partition tree with weighted cases
#' f_auc_rpart(df = df)
#'
#' #AUC of random forest with weighted cases
#' f_auc_rf(df = df)
#' @name f_auc
NULL

#' @autoglobal
#' @rdname f_auc
#' @family preference_order
#' @export
f_auc_glm_binomial <- function(df){

  if(all(sort(unique(df[["y"]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  p <- stats::glm(
    formula = y ~ x,
    data = df,
    family = stats::quasibinomial(
      link = "logit"
      ),
    weights = case_weights(
      x = df[["y"]]
      )
  ) |>
    stats::predict(
      type = "response"
    ) |>
    suppressWarnings() |>
    suppressMessages()

  auc(
    o = df[["y"]],
    p = p
  )

}


#' @autoglobal
#' @rdname f_auc
#' @family preference_order
#' @export
f_auc_glm_binomial_poly2 <- function(df){

  if(all(sort(unique(df[["y"]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  p <- stats::glm(
    formula = y ~ stats::poly(
      x,
      degree = 2,
      raw = TRUE
    ),
    data = df,
    family = stats::quasibinomial(
      link = "logit"
    ),
    weights = case_weights(
      x = df[["y"]]
      )
  ) |>
    stats::predict(
      type = "response"
    ) |>
    suppressWarnings() |>
    suppressMessages()

  auc(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_auc
#' @family preference_order
#' @export
f_auc_gam_binomial <- function(df){

  if(all(sort(unique(df[["y"]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  p <- mgcv::gam(
    formula = y ~ s(x),
    data = df,
    family = stats::quasibinomial(link = "logit"),
    weights = case_weights(
      x = df[["y"]]
      ),
    select = TRUE
  ) |>
    stats::predict(
      type = "response"
    ) |>
    suppressWarnings() |>
    suppressMessages()

  auc(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_auc
#' @family preference_order
#' @export
f_auc_rpart <- function(df){

  if(all(sort(unique(df[["y"]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  p <- rpart::rpart(
    formula = y ~ x,
    data = df,
    weights = case_weights(
      x = df[["y"]])
    ,
    control = rpart::rpart.control(
      minbucket = ceiling(
        nrow(df)/100
      )
    )
  ) |>
    stats::predict(
      type = "vector"
    )

  auc(
    o = df[["y"]],
    p = p
  )

}

#' @autoglobal
#' @rdname f_auc
#' @family preference_order
#' @export
f_auc_rf <- function(df){

  if(all(sort(unique(df[["y"]])) == c(0, 1)) == FALSE){
    stop("Argument 'response' must be the name of a binary vector with unique values 0 and 1.")
  }

  m <- ranger::ranger(
    formula = y ~ x,
    data = df,
    case.weights = case_weights(
      x = df[["y"]]
      ),
    num.threads = 1,
    num.trees = 100,
    min.node.size = ceiling(nrow(df)/100),
    seed = 1
  )

  p <- stats::predict(
    object = m,
    data = df
  )$predictions

  auc(
    o = df[["y"]],
    p = p
  )

}

#' Association Between a Categorical Response and a Categorical Predictor
#'
#' @description
#' Computes Cramer's V, a measure of association between categorical or factor variables. Please see [cramer_v()] for further details.
#'
#' @param df (required, data frame) with columns:
#' \itemize{
#'   \item "x": (character or factor) categorical predictor.
#'   \item "y": (character or factor) categorical response.
#' }
#' @return numeric: Cramer's V
#' @examples
#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #categorical response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_factor"]],
#'   x = vi[["soil_type"]]
#' ) |>
#'   na.omit()
#'
#' #Cramer's V
#' f_cramerv(df = df)
#' @autoglobal
#' @family preference_order
#' @export
f_cramerv <- function(df){

  cramer_v(
    x = df[["x"]],
    y = df[["y"]],
    check_input = FALSE
  )

}

#' Association Between a Categorical Response and a Categorical or Numerical Predictor
#'
#' @description
#' Computes the Cramer's V between a categorical response (of class "character" or "factor") and the prediction of a Random Forest model with a categorical or numeric predictor and weighted cases.
#'
#' @param df (required, data frame) with columns:
#' \itemize{
#'   \item "x": (character, factor, or numeric) categorical or numeric predictor.
#'   \item "y" (character or factor) categorical response.
#' }
#' @return numeric: Cramer's V
#' @examples

#' #load example data
#' data(vi)
#'
#' #reduce size to speed-up example
#' vi <- vi[1:1000, ]
#'
#' #categorical response and predictor
#' #to data frame without NAs
#' df <- data.frame(
#'   y = vi[["vi_factor"]],
#'   x = vi[["soil_type"]]
#' ) |>
#'   na.omit()
#'
#' #Cramer's V of a Random Forest model
#' f_cramerv_rf_categorical(df = df)
#'
#' #categorical response and numeric predictor
#' df <- data.frame(
#'   y = vi[["vi_factor"]],
#'   x = vi[["swi_mean"]]
#' ) |>
#'   na.omit()
#'
#' f_cramerv_rf_categorical(df = df)

#' @autoglobal
#' @family preference_order
#' @export
f_cramerv_rf_categorical <- function(df){

  df[["y"]] <- as.factor(df[["y"]])

  m <- ranger::ranger(
    formula = y ~ x,
    data = df,
    case.weights = case_weights(
      x = df[["y"]]
    ),
    num.threads = 1,
    num.trees = 100,
    min.node.size = ceiling(nrow(df)/100),
    seed = 1
  )

  p <- stats::predict(
    object = m,
    data = df
  )$predictions

  cramer_v(
    x = df[["y"]],
    y = p,
    check_input = FALSE
  )

}


#' @title Case Weights for Unbalanced Binomial or Categorical Responses
#' @param x (required, integer, character, or factor vector) Either a binomial response with 1s and 0s, or categorical responses as characters or factors. Default: `NULL`
#' @return numeric vector: case weights
#' @examples
#'  case_weights(
#'    x = c(0, 0, 0, 1, 1)
#'  )
#'
#'  case_weights(
#'    x = c("a", "a", "b", "b", "c")
#' @family preference_order
#' @autoglobal
#' @export
case_weights <- function(
    x = NULL
){

  # Convert to factor if not already
  x <- as.factor(x)

  # ocurrences per level
  n <- length(x)
  counts <- table(x)

  # weights as inverse of the counts
  weights <- 1 / counts

  # vector of weights
  as.numeric(weights[as.character(x)])

}
