library(dplyr)
library(ggplot2)

load("dev_scripts/equivalence_max_vif_max_cor/experiment_results.RData")

experiment_df <- experiment_df |>
  dplyr::transmute(
    in_columns = n_cols,
    in_rows = n_rows,
    max_cor,
    max_vif = out_max_vif,
    out_selection_length = out_length,
    out_selection_jaccard = out_similarity
  ) |>
  as.data.frame()

usethis::use_data(experiment_df, overwrite = TRUE)

#cor to vif
gam_cor_to_vif <- mgcv::gam(
  formula = max_vif ~ s(max_cor, k = 9),
  weights = experiment_df$out_selection_jaccard^3,
  data = experiment_df,
  select = TRUE
)

usethis::use_data(gam_cor_to_vif, overwrite = TRUE)

newdat <- with(experiment_df, data.frame(max_cor = seq(min(max_cor), max(max_cor), length = 200)))
pred   <- predict(gam_cor_to_vif, newdat, se.fit = TRUE)

ggplot() +
  geom_point(aes(max_cor, max_vif), data = experiment_df, alpha = 0.5) +
  geom_line(aes(max_cor, pred$fit), data = newdat, color = "blue") +
  geom_ribbon(aes(max_cor, ymin = pred$fit - 2*pred$se.fit,
                  ymax = pred$fit + 2*pred$se.fit),
              data = newdat, alpha = 0.2)


#equivalency table
equivalence_cor_vif <- data.frame(
  max_cor = seq(0.1, 1, by = 0.01),
  max_vif = NA
)

equivalence_cor_vif$max_vif <- mgcv::predict.gam(
  object = gam_cor_to_vif,
  newdata = equivalence_cor_vif) |>
  round(digits = 2)

usethis::use_data(equivalence_cor_vif)
