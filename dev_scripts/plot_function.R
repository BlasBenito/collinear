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
  cex = 0.5,
  xlim = c(1, 0),
  )

#other version
#####################################################################

library(dendextend)
k <- 22
m.clust <- color_branches(m.clust, k=k, col = viridis::turbo(n = k))
plot(m.clust, horiz = TRUE)

