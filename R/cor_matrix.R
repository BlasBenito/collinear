#' Absolute Pairwise Correlation Matrix
#'
#' @description
#' Computes a square matrix of absolute pairwise correlations for a set of numeric and/or categorical predictors.
#'
#' If \code{df} is already a correlation data frame (as returned by [cor_df()]), the function converts it directly into a correlation matrix. Otherwise, [cor_df()] is used internally to compute absolute pairwise correlations before creating the matrix.
#'
#' Supports parallel computation via [future::plan()] and optional progress reporting via [progressr::handlers()].
#'
#'
#' @inheritParams collinear
#' @param df (required; data frame, tibble, or sf) A dataframe with predictors or the output of[cor_df()]. Default: NULL.
#' @return correlation matrix
#'
#' @examples
#' data(vi_smol)
#'
#' predictors <- c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
#'
#' #OPTIONAL: parallelization setup
#' # future::plan(
#' #   future::multisession,
#' #   workers = 2
#' # )
#'
#' #OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #from dataframe with predictors
#' m <- cor_matrix(
#'   df = vi_smol,
#'   predictors = predictors
#' )
#'
#' m
#'
#' #from correlation dataframe
#' m <- cor_df(
#'   df = vi,
#'   predictors = predictors
#' ) |>
#'   cor_matrix()
#'
#' m
#'
#' #OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_matrix <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_matrix()",
    ... = ...
  )

  #if df with predictors, compute correlation matrix
  if(
    all(
      names(df) %in% c(
        "x",
        "y",
        "correlation"
        )
      ) == FALSE
    ){

    df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  #create all possible pairs
  df <- rbind(
    df,
    data.frame(
      x = df$y,
      y = df$x,
      correlation = df$correlation
    )
  )

  #rows and col names
  variables <- sort(
    unique(
      c(df$x, df$y)
      )
    )

  #empty square matrix
  m <- matrix(
    data = NA,
    nrow = length(variables),
    ncol = length(variables)
    )

  #named vector to map row/column names to indices
  index_map <- stats::setNames(
    object = seq_along(variables),
    nm = variables
    )

  #vectorized indexing to fill in the matrix
  m[
    cbind(
      index_map[df$x],
      index_map[df$y]
      )
    ] <- df$correlation

  #dim names
  dimnames(m) <- list(
    variables,
    variables
  )

  #replace NA in diag with 1
  diag(m) <- 1

  m

}
