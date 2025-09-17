library(collinear)
library(distantia)
library(ggplot2)

n = 1000
rows <- 30:1000
classes <- seq(2, 10, by = 2)

iterations_df <- data.frame(
  rows = sample(x = rows, size = n, replace = TRUE),
  classes = sample(x = classes, size = n, replace = TRUE),
  cramer_v = rep(NA, n),
  cor = rep(NA, n)
)

for(i in seq_len(nrow(iterations_df))){

  data.i <- distantia::zoo_simulate(
    rows = iterations_df[i, "rows"],
    time_range = c(1, iterations_df[i, "rows"]),
    cols = 2,
    data_range = c(0, iterations_df[i, "classes"]),
    irregular = FALSE
  ) |>
    as.matrix()

  x <- as.integer(data.i[, 1])
  y <- as.integer(data.i[, 2])

  iterations_df[i, "cor"] <- stats::cor(
    x = x,
    y = y
  ) |>
    abs()

  iterations_df[i, "cramer_v"] <- collinear::cor_cramer_v(
    x = x,
    y = y
  )

}

ggplot(iterations_df) +
  aes(
    x = cor,
    y = cramer_v,
    group = factor(classes),
    color = factor(classes)
  ) +
  geom_point() +
  geom_smooth()
