#' @title Automated Multicollinearity Filtering
#'
#' @description
#' Automatizes multicollinearity filtering via absolute pairwise correlation and/or variance inflation factors in dataframes with numeric and categorical predictors.
#'
#' The argument \code{max_cor} determines the maximum variance inflation factor allowed in the resulting selection of predictors.
#'
#' The argument \code{max_vif} determines the maximum variance inflation factor allowed in the resulting selection of predictors.
#'
#' The argument \code{preference_order} accepts a character vector of predictor names ranked from first to last index, or a dataframe resulting from [preference_order()]. When two predictors in this vector or dataframe are highly collinear, the one with a lower ranking is removed. This option helps protect predictors of interest. If not provided, predictors are ranked from lower to higher multicollinearity.
#' Please check the sections **Variance Inflation Factors**, **VIF-based Filtering**, and **Pairwise Correlation Filtering** at the end of this help file for further details.
#'
#'
#' @inheritSection collinear Pairwise Correlation Filtering
#' @inheritSection collinear Variance Inflation Factors
#' @inheritSection collinear VIF-based Filtering
#'
#' @inheritParams collinear
#' @param response (optional; character or NULL) Name of one response variable in \code{df}. Used to exclude columns when \code{predictors} is NULL, and to filter \code{preference_order} when it is a dataframe and contains several responses. Default: NULL.
#' @param max_cor (optional; numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. Valid values are between 0.01 and 0.99, and recommended values are between 0.5 (strict) and 0.9 (permissive). Default: 0.7
#'
#' @param max_vif (optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2.5 (strict) and 10 (permissive). Default: 5
#' @return character vector: names of selected predictors
#' @examples
#' data(vi_smol)
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
#' #predictors
#' predictors = c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
#'
#' #predictors ordered from lower to higher multicollinearity
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' x
#'
#'
#' #with incomplete preference order vector
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = c(
#'     "koppen_zone",
#'     "soil_type"
#'   ),
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' x
#'
#' #with complete preference order vector
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = c(
#'     "soil_temperature_mean",
#'     "koppen_zone",
#'     "soil_type",
#'     "topo_elevation"
#'   ),
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' x
#'
#' #with preference order dataframe
#' df_preference <- preference_order(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = predictors
#' )
#'
#' df_preference
#'
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = df_preference,
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' x
#'
#' #identical to cor_select()
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = df_preference,
#'   max_cor = 0.7,
#'   max_vif = NULL
#' )
#'
#' x
#'
#' #identical to vif_select()
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = predictors,
#'   preference_order = df_preference,
#'   max_cor = NULL,
#'   max_vif = 5
#' )
#'
#' x
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @author Blas M. Benito, PhD
#' @export
collinear_select <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    max_cor = 0.7,
    max_vif = 5,
    quiet = FALSE,
    ...
){

  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear_select()",
    function_name = dots$function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  response <- validate_arg_responses(
    df = df,
    responses = response,
    max_responses = 1,
    quiet = quiet,
    function_name = function_name
  )

  predictors <- validate_arg_predictors(
    df = df,
    responses = response,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  df.ncol <- ncol(df)

  df <- validate_arg_df(
    df = df,
    responses = response,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #revalidate predictors if any columns were removed
  if(ncol(df) < df.ncol){

    attributes(response)$validated <- NULL
    attributes(predictors)$validated <- NULL

    response <- validate_arg_responses(
      df = df,
      responses = response,
      max_responses = 1,
      quiet = quiet,
      function_name = function_name
    )

    predictors <- validate_arg_predictors(
      df = df,
      responses = response,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  #univariate case
  if(length(predictors) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one valid predictor in 'predictors', skipping multicollinearity filtering."
      )

    }

    return(predictors)

  }

  if(is.null(max_cor) && is.null(max_vif)){

    stop(
      "\n",
      function_name,
      ": arguments 'max_cor' and 'max_vif' cannot be NULL at once.",
      call. = FALSE
    )

  }

  m <- cor_matrix(
    df = df,
    predictors = predictors,
    function_name = function_name,
    m = dots$m
  )

  max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  preference.order <- validate_arg_preference_order(
    df = df,
    response = response,
    predictors = predictors,
    preference_order = preference_order,
    quiet = quiet,
    function_name = function_name,
    m = m
  )

  preference.order <- preference.order$predictor

  #organize the correlation matrix according to preference_order
  m.class <- class(m)
  m <- m[
    preference.order,
    preference.order
  ]
  class(m) <- m.class

  # Check if filtering is needed
  skip_cor <- FALSE
  skip_vif <- FALSE

  #check cor threshold
  if(!is.null(max_cor)){

    if(max(m[upper.tri(m)]) <= max_cor){

      skip_cor <- TRUE

      if(quiet == FALSE && is.null(max_vif)){

        message(
          "\n",
          function_name,
          ": maximum pairwise correlation is <= ", max_cor,
          ", multicollinearity filtering is not required."
        )

        return(predictors)

      }
    }
  }

  if(!is.null(max_vif)) {

    current_max_vif <- max(
      vif(
        m = m,
        quiet = quiet,
        function_name = function_name
      )
    )

    #check vif threshold
    if(current_max_vif <= max_vif){

      skip_vif <- TRUE

      if(quiet == FALSE && is.null(max_cor)) {

        message(
          "\n",
          function_name,
          ": maximum VIF is <= ", max_vif, ", multicollinearity filtering is not required."
        )

        return(predictors)

      }
    }
  }

  #both are below thresholds
  if(skip_cor && skip_vif){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": multicollinearity is below 'max_cor' and 'max_vif', filtering is not required."
      )

    }

    return(predictors)

  }

  #filtering loop
  selected <- preference.order[1]
  candidates <- preference.order[-1]

  #iterate over candidates
  for(candidate in candidates) {

    #correlation criterion (if applicable)
    cor_ok <- TRUE

    if(!is.null(max_cor) && !skip_cor){

      cor_ok <- max(m[selected, candidate]) <= max_cor

    }

    #VIF criterion, only checked if cor passed
    vif_ok <- TRUE

    if(!is.null(max_vif) && !skip_vif && cor_ok){

      selected_columns <- c(
        selected,
        candidate
      )

      current_vif <- vif(
        m = m[
          selected_columns,
          selected_columns
        ],
        quiet = quiet,
        function_name = function_name
      ) |>
        max()

      vif_ok <- current_vif <= max_vif

    }

    #add candidate if all criteria is TRUE
    if(cor_ok && vif_ok){

      selected <- c(selected, candidate)

    }

  }

  attr(
    x = selected,
    which = "validated"
  ) <- TRUE

  selected

}
