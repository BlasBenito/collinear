# creates the toy data frame
library(collinear)
library(dplyr)

set.seed(1)
toy <- vi |>
  dplyr::slice_sample(n = 2000) |>
  dplyr::transmute(
    a = soil_clay,
    b = humidity_range
  ) |>
  scale() |>
  as.data.frame() |>
  dplyr::mutate(
    y = a * 0.75 + b * 0.25 + runif(n = dplyr::n(), min = -0.5, max = 0.5),
    c = a + runif(n = dplyr::n(), min = -0.5, max = 0.5),
    d = (a + b) / 2 + runif(n = dplyr::n(), min = -0.5, max = 0.5)
  ) |>
  dplyr::transmute(y, a, b, c, d)

usethis::use_data(toy, overwrite = TRUE)
