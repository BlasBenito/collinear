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
#'     data(vi)
#'
#'     #subset to speed-up example
#'     vi <- vi[1:1000, ]
#'
#'     predictors <- c(
#'       "koppen_zone", #character
#'       "soil_type", #factor
#'       "topo_elevation", #numeric
#'       "soil_temperature_mean" #numeric
#'     )
#'
#'     #OPTIONAL: parallelization setup
#'     # future::plan(
#'     #   future::multisession,
#'     #   workers = 2
#'     # )
#'
#'     #OPTIONAL: progress bar
#'     # progressr::handlers(global = TRUE)
#'
#'
#'     #correlation data frame to matrix
#'     m <- cor_df(
#'       df = vi,
#'       predictors = predictors
#'     ) |>
#'       cor_matrix()
#'
#'     m
#'
#'     #direct computation (uses cor_df() internally)
#'     m <- cor_matrix(
#'       df = vi,
#'       predictors = predictors
#'     )
#'
#'     m
#'
#'     #OPTIONAL: disable parallelization
#'     #future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @author Blas M. Benito, PhD
#' @export
cor_matrix <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE
){

  function_name <- "collinear::cor_matrix()"

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

    df <- validate_arg_df(
      df = df,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet
    )

    predictors <- validate_arg_predictors_cor(
      df = df,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet
    )

    df <- cor_df(
      df  = df,
      predictors = predictors,
      quiet = quiet
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
