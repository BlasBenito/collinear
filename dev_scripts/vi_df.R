library(collinear)
library(dplyr)

data(vi)

colnames(vi)

vi$vi_factor <- as.factor(vi$vi_category)

vi <- vi |>
  dplyr::relocate(
    vi_factor,
    .after = vi_category
  )

vi$soil_type <- as.factor(vi$soil_type)

vi <- vi |>
  dplyr::mutate(
    vi_numeric = vi_mean,
    vi_counts = as.integer(vi_mean * 1000),
    vi_category = dplyr::case_when(
      vi_mean >= quantile(vi_mean, 0.8)  ~ "very_high",
      vi_mean >= quantile(vi_mean, 0.6)  ~ "high",
      vi_mean >= quantile(vi_mean, 0.4)  ~ "medium",
      vi_mean >= quantile(vi_mean, 0.2)  ~ "low",
      TRUE ~ "very_low"
    )
  ) |>
  dplyr::relocate(
    vi_numeric,
    vi_counts,
    vi_binary,
    vi_category,
    .before = vi_mean
  ) |>
  dplyr::select(
    -vi_mean,
    -vi_max,
    -vi_min,
    -vi_range
  )

usethis::use_data(vi, overwrite = TRUE)


vi_predictors_numeric <- identify_numeric_predictors(
  df = vi,
  predictors = vi_predictors
)

usethis::use_data(vi_predictors_numeric)

vi_predictors_category <- identify_non_numeric_predictors(
  df = vi,
  predictors = vi_predictors
)


usethis::use_data(vi_predictors_category)
