library(dplyr)
library(usethis)
library(collinear)

data(vi)

vi <- vi |>
  dplyr::mutate(
    vi_binary = dplyr::case_when(
      vi_mean > 0.5 ~ 1,
      vi_mean <= 0.5 ~ 0
    )
  ) |>
  dplyr::relocate(
    vi_binary,
    .after = vi_range
  )

usethis::use_data(vi, overwrite = TRUE)
