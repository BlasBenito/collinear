#' Hierarchical Clustering from a Pairwise Correlation Matrix
#'
#' @description
#'
#' Hierarchical clustering of predictors from their pairwise correlation matrix. Computes the correlation matrix with [cor_df()] and [cor_matrix()], transforms it to a dist object, computes a clustering solution with [stats::hclust()], and applies [stats::cutree()] to separate groups based on the value of the argument `max_cor`.
#'
#' Returns a data frame with predictor names and their clusters, and optionally, prints a dendrogram of the clustering solution.
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams collinear
#' @param method (optional, character string) Argument of [stats::hclust()] defining the agglomerative method. One of: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Unambiguous abbreviations are accepted as well. Default: "complete".
#' @param plot (optional, logical) If TRUE, the clustering is plotted. Default: FALSE
#'
#' @return data frame: predictor names and their clusters
#' @examples
#'
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar
#' # progressr::handlers(global = TRUE)
#'
#' df_clusters <- cor_clusters(
#'   df = vi[1:1000, ],
#'   predictors = vi_predictors[1:15]
#' )
#'
#' #disable parallelization
#' future::plan(future::sequential)
#'
#' @export
#' @family pairwise_correlation
#' @autoglobal
cor_clusters <- function(
    df = NULL,
    predictors = NULL,
    max_cor = 0.75,
    method = "complete",
    plot = FALSE
){

  m <- cor_matrix(
    df = df,
    predictors = predictors
  )

  m <- stats::as.dist(1 - abs(m))

  hc <- stats::hclust(
    d = m,
    method = method
  )

  hc_groups <- stats::cutree(
    tree = hc,
    h = 1 - max_cor,
  )

  df_clusters <- data.frame(
    predictor = names(hc_groups),
    cluster = hc_groups
  )

  rownames(df_clusters) <- NULL

  if(plot == TRUE){

    plot(
      x = hc,
      labels = df_clusters$predictor,
      main = paste0("Clustered predictors - max_cor = ", max_cor),
      xlab = "",
      ylab = "1 - Cor",
      sub = "",
      cex = 0.8,
      hang = -1
    )

    graphics::abline(
      h = 1 - max_cor,
      col = "gray50",
      lty = 3,
      lwd = 2
      )

    stats::rect.hclust(
      tree = hc,
      h = 1 - max_cor,
      border = "red4"
    )

  }

  df_clusters <- df_clusters[order(df_clusters$cluster), ]

  rownames(df_clusters) <- NULL

  df_clusters

}
