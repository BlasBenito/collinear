#' Pairwise Correlation Matrix
#'
#' @description
#' If argument 'df' results from [cor_df()], transforms it to a correlation matrix. If argument 'df' is a dataframe with predictors, and the argument 'predictors' is provided then [cor_df()] is used to compute pairwise correlations, and the result is transformed to matrix.
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams collinear
#' @return correlation matrix
#'
#' @examples
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
#'
#' #mixed predictors
#' vi_predictors <- vi_predictors[1:10]
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
#' #correlation data frame
#' df <- cor_df(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' df
#'
#' #correlation matrix
#' m <- cor_matrix(
#'   df = df
#' )
#'
#' m
#'
#' #generating it from the original data
#' m <- cor_matrix(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' m
#'
#' #disable parallelization
#' future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_matrix <- function(
    df = NULL,
    predictors = NULL
){

  #if df with predictors, compute cor data frame
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
      predictors = predictors
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
    ] <- abs(df$correlation)

  #dim names
  dimnames(m) <- list(
    variables,
    variables
  )

  #replace NA in diag with 1
  diag(m) <- 1

  m

}
