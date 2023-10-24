library(collinear)
data(vi, vi_predictors)

vif <- vif_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

cor <- cor_df(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

#plot starts here
m <- cor_matrix(
  df = vi,
  response = "vi_mean",
  predictors = vi_predictors
)

m <- m |>
  abs() |>
  as.dist()

m.clust <- hclust(
  d = 1 - m,
  method = "mcquitty"
) |>
  as.dendrogram()

par(mar = c(4, 1, 1, 10), cex = 0.6)
plot(
  m.clust,
  horiz = TRUE,
  cex = 0.1,
  xlim = c(1, 0)
  )

cut_height <- 0.4  # Specify your desired cut height
m.clust <- m.clust |>
  dendextend::cutr (cut_height, k = NULL)


#cut at certain level for horizontal hclust plots
max_cor <- 0.4
distance_threshold <- 1 - max_cor

k <- cutree(
  tree = m.clust,
  h = distance_threshold
  ) |>
  unique() |>
  length()


plot(m.clust)
rect.hclust(m.clust, k = k, border = "red")
