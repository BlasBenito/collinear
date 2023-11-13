data(vi)

x = "soil_nitrogen"
y = "vi_binary"
df = vi

#for balanced designs
f_logistic_auc_balanced <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- glm(
    formula = y ~ x,
    data = data,
    family = binomial(link = "logit")
  ) |>
    suppressWarnings()

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}

f_logistic_auc_unbalanced <- function(x, y, df){

  data <- data.frame(
    y = df[[y]],
    x = df[[x]]
  ) |>
    na.omit()

  m <- glm(
    formula = y ~ x,
    data = data,
    family = stats::quasibinomial(link = "logit"),
    weights = case_weights(x = data$y)
  ) |>
    suppressWarnings()

  auc_score(
    observed = data$y,
    predicted = stats::predict(m, type = "response")
  )

}

