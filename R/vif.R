#' Variance Inflation Factor from Correlation Matrix
#'
#' @description
#' Internal function used within [vif_df()] to compute Variance Inflation Factors from a correlation matrix. Direct usage of this function is not recommended.
#'
#'
#' @param m (required, numeric matrix) Correlation matrix. Default: NULL
#'
#' @return data frame: variance inflation factors
#' @export
#' @autoglobal
#' @examples
#' data(
#'   vi,
#'   vi_predictors_numeric
#' )
#'
#' #compute correlation matrix
#' m <- stats::cor(
#'   x = vi[, vi_predictors_numeric[1:10]],
#'   use = "complete.obs"
#' )
#'
#' #compute VIF
#' vif(m = m)
vif <- function(m = NULL){

  if(is.null(m)){
    stop(
      "collinear::vif(): argument 'm' cannot be NULL",
      call. = FALSE
    )
  }

  if(capabilities("long.double") == TRUE){
    tolerance = 0
  } else {
    tolerance = .Machine$double.eps
  }

  #compute VIF
  df <- m |>
    solve(tol = tolerance) |>
    diag() |>
    data.frame(stringsAsFactors = FALSE)

  #format data frame
  colnames(df) <- "vif"
  df$vif <- round(abs(df$vif), 4)
  df$variable <- colnames(m)
  rownames(df) <- NULL

  #arrange by VIF
  df[
    order(df$vif),
    c("variable", "vif")
    ]

}
