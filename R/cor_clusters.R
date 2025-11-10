#' Hierarchical Correlation Clustering
#'
#' @description
#'
#' Hierarchical clustering of predictors from their absolute correlation matrix. Computes the correlation matrix with [cor_df()] and [cor_matrix()], transforms it to a distance matrix using [stats::dist()], computes a clustering solution with [stats::hclust()], and applies [stats::cutree()] to separate groups based on the value of the argument \code{max_cor}.
#'
#' Returns a dataframe with predictor names and their clusters, and optionally, prints a dendrogram of the clustering solution.
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams cor_matrix
#' @param max_cor (optional; numeric or NULL) Absolute correlation values used to separate clustering groups. Valid values are between 0.01 and 0.99. Default: 0.7
#' @param method (optional, character string) Argument of [stats::hclust()] defining the agglomerative method. One of: "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Unambiguous abbreviations are accepted as well. Default: "complete".
#'
#' @return list:
#' \itemize{
#'   \item df: dataframe with predictor names and their cluster IDs.
#'   \item hclust: clustering object
#' }
#'
#' @examples
#'data(vi_smol)
#'
#' ## OPTIONAL: parallelization setup
#' ## irrelevant when all predictors are numeric
#' ## only worth it for large data with many categoricals
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' ## OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #group predictors using max_cor as clustering threshold
#' clusters <- cor_clusters(
#'   df = vi_smol,
#'   predictors = c(
#'     "koppen_zone", #character
#'     "soil_type", #factor
#'     "topo_elevation", #numeric
#'     "soil_temperature_mean" #numeric
#'   ),
#'   max_cor = 0.75
#' )
#'
#' #clusters dataframe
#' clusters$df
#'
#' ##plot hclust object
#' # graphics::plot(clusters$hclust)
#'
#' ##plot max_cor threshold
#' # graphics::abline(
#' #   h = 1 - 0.75,
#' #   col = "red4",
#' #   lty = 3,
#' #   lwd = 2
#' # )
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @export
#' @family pairwise_correlation
#' @autoglobal
cor_clusters <- function(
    df = NULL,
    predictors = NULL,
    max_cor = 0.75,
    method = "complete",
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

  m <- stats::as.dist(1 - m)

  hc <- tryCatch(
    stats::hclust(d = m, method = method),
    error = function(e) {
      stop(
        function_name <- validate_arg_function_name(
          default_name = "stats::hclust()",
          function_name = function_name
          ),
        ": clustering failed: ",
        e$message,
        call. = FALSE
        )
    }
  )

  hc_groups <- stats::cutree(
    tree = hc,
    h = 1 - max_cor,
  )

  df_clusters <- data.frame(
    predictor = names(hc_groups),
    cluster = hc_groups
  )

  df_clusters <- df_clusters[order(df_clusters$cluster), ]

  rownames(df_clusters) <- NULL

  out <- list(
    df = df_clusters,
    hclust = hc
  )

  out


}
