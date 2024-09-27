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


f_rpart_binomial_unbalanced_auc(x, y, df)

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



f_glm_gaussian_poly2 <- function(x, y, df){

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


vi$vi_mean_count <- as.integer(vi$vi_mean * 1000)

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
    y = stats::predict(m)
  )^2

}
