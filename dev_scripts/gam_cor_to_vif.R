library(collinear)
library(ggplot2)

data(experiment_cor_vs_vif)

#check data and model
experiment_cor_vs_vif |>
  dplyr::arrange(
    selection_similarity
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = input_max_cor,
    y = output_max_vif,
    color = selection_similarity,
    weight = selection_similarity^3
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 9),
    method.args = list(
      select = TRUE
    ),
    color = "black",
    se = TRUE
  ) +
  ggplot2::scale_color_viridis_c(
    option = "turbo",
    direction = 1
  ) +
  ggplot2::labs(
    title = "Experiment Pearson Correlation vs. VIF",
    x = "Input max_cor",
    y = "max_vif leading to most similar selection",
    color = "Jaccard\nsimilarity\nbetween\nselections"
  ) +
  ggplot2::theme_bw()

df <- experiment_cor_vs_vif |>
  dplyr::rename(
    max_cor = input_max_cor,
    max_vif = output_max_vif
  )


gam_cor_to_vif <- mgcv::gam(
  formula = max_vif ~ s(max_cor, k = 9),
  weights = df$selection_similarity^3,
  data = df,
  select = TRUE
)

usethis::use_data(gam_cor_to_vif, overwrite = TRUE)

