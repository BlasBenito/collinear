#' Variance Inflation Factors Dataframe
#'
#' @description
#'
#' Computes the pairwise correlation matrix between all pairs of predictors via [cor_df()] and [cor_matrix()], applies [vif()] to the resulting matrix to compute Variance Inflation Factors, and returns the result as a dataframe.
#'
#' @inheritSection collinear Variance Inflation Factors
#'
#' @inheritParams collinear
#' @return dataframe with columns:
#' \itemize{
#'   \item \code{predictor}: Character, predictor name.
#'   \item \code{vif}: Numeric, variance inflation factor
#' }
#'
#' @examples
#'
#' data(vi_smol)
#'
#' # ## OPTIONAL: parallelization setup
#' # ## irrelevant when all predictors are numeric
#' # ## only worth it for large data with many categoricals
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' # ## OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #predictors
#' predictors = c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
#'
#' x <- vif_df(
#'   df = vi_smol,
#'   predictors = predictors
#' )
#'
#' x
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#'
#' @autoglobal
#' @family variance_inflation_factor
#' @inherit vif_select references
#' @export
vif_df <- function(
  df = NULL,
  predictors = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::vif_df()",
    function_name = dots$function_name
  )

  if (!inherits(dots$m, "collinear_cor_matrix")) {
    df <- validate_arg_df_not_null(
      df = df,
      function_name = function_name
    )

    quiet <- validate_arg_quiet(
      quiet = quiet,
      function_name = function_name
    )

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    df.ncol <- ncol(df)

    df <- validate_arg_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    #revalidate predictors if any columns were removed
    if (ncol(df) < df.ncol) {
      attributes(predictors)$validated <- NULL

      predictors <- validate_arg_predictors(
        df = df,
        predictors = predictors,
        quiet = quiet,
        function_name = function_name
      )
    }

    #if no predictors
    if (length(predictors) == 1) {
      if (quiet == FALSE) {
        message(
          "\n",
          function_name,
          ": only one valid predictor, returning one-row dataframe."
        )
      }

      return(
        data.frame(
          variable = predictors,
          vif = 0
        )
      )
    }
  }

  #compute correlation matrix
  m <- cor_matrix(
    df = df,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet,
    m = dots$m
  )

  out <- vif(
    m = m,
    quiet = quiet,
    function_name = function_name
  ) |>
    data.frame(
      stringsAsFactors = FALSE
    )

  #format dataframe
  colnames(out) <- "vif"
  out$predictor <- colnames(m)
  rownames(out) <- NULL

  class(out) <- c("collinear_vif_df", class(out))

  out
}
