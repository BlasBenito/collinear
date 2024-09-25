data(vi)

x = "soil_nitrogen"
y = "vi_mean"
df = vi

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
