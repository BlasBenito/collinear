#' Hierarchical Clustering from Pairwise Correlation Matrix
#'
#' @description
#'
#' Hierarchical clustering of predictors from their pairwise correlation matrix. Computes the correlation matrix with [cor_df()] and [cor_matrix()], transforms it to a distance matrix using [stats::dist()], computes a clustering solution with [stats::hclust()], and applies [stats::cutree()] to separate groups based on the value of the argument \code{max_cor}.
#'
#' Returns a data frame with predictor names and their clusters, and optionally, prints a dendrogram of the clustering solution.
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams collinear
#' @param method (optional, character string) Argument of [stats::hclust()] defining the agglomerative method. One of: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Unambiguous abbreviations are accepted as well. Default: "complete".
#' @param plot (optional, logical) If TRUE, the clustering is plotted. Default: FALSE
#'
#' @return data frame: predictor names and their cluster IDs
#'
#' @examples
#'   data(vi)
#'
#'   #subset to speed-up example
#'   vi <- vi[1:1000, ]
#'
#'   #OPTIONAL: parallelization setup
#'   # future::plan(
#'   #   future::multisession,
#'   #   workers = 2
#'   # )
#'
#'   #OPTIONAL: progress bar
#'   # progressr::handlers(global = TRUE)
#'
#'   #group predictors using max_cor as clustering threshold
#'   df_clusters <- cor_clusters(
#'     df = vi,
#'     predictors = c(
#'       "koppen_zone", #character
#'       "soil_type", #factor
#'       "topo_elevation", #numeric
#'       "soil_temperature_mean" #numeric
#'     ),
#'     max_cor = 0.75,
#'     plot = FALSE #set to TRUE to plot result
#'   )
#'
#'   #OPTIONAL: disable parallelization
#'   #future::plan(future::sequential)
#' @export
#' @family pairwise_correlation
#' @autoglobal
cor_clusters <- function(
    df = NULL,
    predictors = NULL,
    max_cor = 0.75,
    method = "complete",
    plot = FALSE,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_clusters()",
    ... = ...
  )

  m <- cor_matrix(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
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
