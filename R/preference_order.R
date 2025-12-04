#' Quantitative Prioritization for Multicollinearity Filtering
#'
#' @description
#' Generates a valid input for the argument \code{preference_order} of the functions [vif_select()], [cor_select()], [collinear_select()], and [collinear()]. This argument helps preserve important predictors during multicollinearity filtering.
#'
#' The function works in two different ways:
#' \itemize{
#'   \item When \code{f} is NULL, it ranks the predictors from lower to higher multicollinearity, computed as one minus the average Pearson correlation between the given predictor against all others. This option is useful when the goal is to limit redundancy in a large dataset and there is not an specific model to train in mind.
#'   \item When \code{responses} and \code{f} are not NULL, it ranks the predictors by the strength of their association with a response based on the evaluation of univariate models. This is the best possible option when the end-goal is training a model.
#' }
#'
#' The argument \code{f} (requires a valid \code{resopnses} argument) defines how the strength of association between the response and each predictor is computed. By default it calls [f_auto()], which uses [f_auto_rules()] to select a suitable function depending on the types of the response and the predictors. This option is designed to provide sensible, general-purpose defaults optimized for speed and stability rather than any specific modeling approach.
#'
#' For more fine-tuned control, the package offers the following \code{f} functions (see [f_functions()]):
#'
#' \itemize{
#'   \item **Numeric response**:
#'   \itemize{
#'    \item [f_numeric_glm()]: Pearson's R-squared of response versus the predictions of a Gaussian GLM.
#'    \item [f_numeric_gam()]: GAM model fitted with [mgcv::gam()].
#'    \item [f_numeric_rf()]: Random Forest model fitted with [ranger::ranger()].
#'   }
#'   \item **Integer counts response**:
#'   \itemize{
#'     \item [f_count_glm()]: Pearson's R-squared of a Poisson GLM.
#'     \item [f_count_gam()]: Poisson GAM.
#'     \item [f_count_rf()]: Random Forest model fitted with [ranger::ranger()].
#'   }
#'   \item **Binomial response (1 and 0)**:
#'   \itemize{
#'     \item [f_binomial_glm()]: AUC of Quasibinomial GLM with weighted cases.
#'     \item [f_binomial_gam()]: AUC of Quasibinomial GAM with weighted cases.
#'     \item [f_binomial_rf()]: AUC of a Random Forest model with weighted cases.
#'   }
#'   \item **Categorical response**:
#'   \itemize{
#'     \item [f_categorical_rf()]: Cramer's V of the response against the predictions of a classification Random Forest model.
#'   }
#' }
#'
#' These functions accept a cross-validation setup via the arguments \code{cv_iterations} and \code{cv_training_fraction}.
#'
#' Additionally, the argument \code{f} accepts any custom function taking a dataframe with the columns "x" (predictor) and "y" (response) and returning a numeric indicator of association.
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' Accepts a character vector of response variables as input for the argument \code{responses}. When more than one response is provided, the output is a named list of preference data frames.
#'
#' @inheritParams collinear
#'
#' @param f (optional: function name) Unquoted function name without parenthesis (see [f_functions]). By default calls to [f_auto()], which selects a suitable function depending on the nature of the response and predictors. Set to NULL if \code{responses = NULL}. If NULL, predictors are ranked from lower to higher multicollinearity. Default: \code{f_auto}
#'
#' @param cv_training_fraction (optional, numeric) Value between 0.1 and 1 defining the training faction used in cross-validation. If 1 (default), no cross-validation is performed, and the resulting metric is computed from all observations and predictions. Automatically set to 1 when \code{cv_iterations = 1}. Default: 1
#'
#' @param cv_iterations (optional, integer) Number of cross-validation iterations to perform. The recommended range lies between 30 and 100. In general, smaller datasets and large values of \code{cv_training_fraction} require more iterations to achieve stability. Automatically set to 1 when \code{cv_training_fraction = 1}. Default: 1
#'
#' @param seed (optional, integer) Random seed, required for reproducibility when using cross-validation or random forest models. Default: 1
#'
#'
#' @family preference_order
#' @return dataframe:
#' \itemize{
#'   \item \code{response}: character, response name, if any, or \code{"none"} otherwise.
#'
#'   \item \code{predictor}: character, name of the predictor.
#'
#'   \item \code{f}: name of the function used to compute the preference order. If argument \code{f} is NULL, the value "stats::cor()" is added to this column.
#'
#'   \item \code{metric}: name of the metric used to assess strength of association. Usually one of "R-squared", "AUC" (Area Under the ROC Curve), or "Cramer's V". If \code{f} is a custom function not in [f_functions()], then \code{metric} is set to "custom". If \code{f} is NULL, then "1 - R-squared" is returned in this column.
#'
#'   \item \code{score}: value of the metric returned by \code{f} to assess the association between the \code{response} and each given \code{predictor}.
#'
#' \item \code{rank}: integer value indicating the rank of the predictor.

#' }
#' @examples
#' #load example data
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#' )
#'
#' ##OPTIONAL: parallelization setup
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' ##OPTIONAL: progress bar
#' ##does not work in R examples
#' # progressr::handlers(global = TRUE)
#'
#' #ranking predictors from lower to higher multicollinearity
#' #------------------------------------------------
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = NULL, #default value
#'   predictors = vi_predictors_numeric[1:10],
#'   f = NULL #must be explicit
#' )
#'
#' x
#'
#' #automatic selection of ranking function
#' #------------------------------------------------
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = c("vi_numeric", "vi_categorical"),
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_auto
#'   )
#'
#' x
#'
#' #user selection of ranking function
#' #------------------------------------------------
#' #Poisson GLM for a integer counts response
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = "vi_binomial",
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_binomial_glm
#' )
#'
#' x
#'
#' #cross-validation
#' #------------------------------------------------
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = "vi_binomial",
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_binomial_glm,
#'   cv_training_fraction = 0.5,
#'   cv_iterations = 10
#' )
#'
#' x
#'
#' #custom pairwise correlation function
#' #------------------------------------------------
#' #custom functions need the ellipsis argument
#' f_rsquared <- function(df, ...){
#'     stats::cor(
#'       x = df$x,
#'       y = df$y,
#'       use = "complete.obs"
#'     )^2
#' }
#'
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_rsquared
#' )
#'
#' x
#'
#' #resetting to sequential processing
#' #future::plan(future::sequential)
#' @autoglobal
#' @author Blas M. Benito, PhD
#' @export
preference_order <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  f = f_auto,
  cv_training_fraction = 1,
  cv_iterations = 1,
  seed = 1,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::preference_order()",
    function_name = dots$function_name
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  #check input dataframe
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  #managing multiple responses
  responses <- validate_arg_responses(
    df = df,
    responses = responses,
    quiet = quiet,
    function_name = function_name
  )

  predictors <- validate_arg_predictors(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  df.ncol <- ncol(df)

  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #revalidate predictors if any columns were removed
  if (ncol(df) < df.ncol) {
    attributes(responses)$validated <- NULL
    attributes(predictors)$validated <- NULL

    response <- validate_arg_responses(
      df = df,
      responses = responses,
      max_responses = 1,
      quiet = quiet,
      function_name = function_name
    )

    predictors <- validate_arg_predictors(
      df = df,
      responses = responses,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )
  }

  #check f
  if (
    is.null(responses) &&
      !is.null(f)
  ) {
    f <- NULL
  }

  f <- validate_arg_f(
    f = f,
    f_name = deparse(substitute(f)),
    function_name = function_name
  )

  #DEFAULT
  if (is.null(f)) {
    if (is.null(responses)) {
      responses <- "none"
    }

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": ranking ",
        length(predictors),
        " 'predictors' from lower to higher multicollinearity."
      )
    }

    m <- cor_matrix(
      df = df,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet,
      m = dots$m
    ) |>
      abs()

    diag(x = m) <- 0

    m.colmeans <- sort(
      x = 1 - colMeans(m),
      decreasing = TRUE
    ) |>
      round(digits = 4)

    m.names <- names(m.colmeans)

    names(m.colmeans) <- NULL

    out <- data.frame(
      response = sort(
        rep(
          x = responses,
          times = length(m.names) * length(responses)
        )
      ),

      predictor = rep(
        x = m.names,
        times = length(responses)
      ),

      f = rep(
        x = "stats::cor()",
        times = length(m.names) * length(responses)
      ),

      metric = rep(
        x = "1 - R-squared",
        times = length(m.names) * length(responses)
      ),

      score = rep(
        x = m.colmeans,
        times = length(responses)
      ),

      rank = rep(
        x = seq_len(length.out = length(m.names)),
        times = length(responses)
      )
    )

    return(out)
  }

  if (
    !is.numeric(cv_iterations) ||
      cv_iterations < 1 ||
      cv_iterations != floor(cv_iterations)
  ) {
    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": argument 'cv_iterations' must be a positive integer, setting it to '1'"
      )
    }

    cv_iterations <- 1
    cv_training_fraction <- 1
  }

  #check cross validation arguments
  if (
    !is.numeric(cv_training_fraction) ||
      cv_training_fraction < 0.1 ||
      cv_training_fraction > 1
  ) {
    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": argument 'cv_training_fraction' must be a numeric between 0.1 and 1 (inclusive), setting it to '1'."
      )
    }

    cv_training_fraction <- 1
    cv_iterations <- 1
  }

  cv_training_fraction <- cv_training_fraction[1]
  cv_iterations <- as.integer(cv_iterations[1])

  if (
    !is.numeric(seed) ||
      seed != floor(seed)
  ) {
    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": argument 'seed' is invalid, resetting it to '1'.",
        call. = FALSE
      )
    }

    seed <- 1L
  }

  f_df <- f_functions()[, c("name", "metric")]
  colnames(f_df) <- c("f", "metric")

  #output list
  out_list <- list()

  #iterating over responses
  for (response in responses) {
    set.seed(seed)

    if (
      quiet == FALSE &&
        length(responses) > 1
    ) {
      msg <- paste0(
        function_name,
        ": processing response '",
        response,
        "'."
      )

      msg_length <- nchar(msg)

      message("\n", msg)
      message(rep(x = "-", times = nchar(msg)))
    }

    #use f_auto to get function name
    if (
      all(c("df", "response", "predictors", "quiet") %in% names(formals(f)))
    ) {
      #get function name
      f_name <- f(
        df = df,
        response = response,
        predictors = predictors,
        quiet = quiet,
        function_name = function_name
      )

      #validate to function
      f.response <- validate_arg_f(
        f = get(f_name),
        f_name = f_name,
        function_name = function_name
      )
    } else {
      f.response <- f
    }

    #dataframe to store results
    preference <- data.frame(
      response = rep(
        x = response,
        times = length(predictors)
      ),
      predictor = predictors,
      f = rep(
        x = attributes(f.response)$name,
        times = length(predictors)
      )
    )

    #progress bar
    p <- progressr::progressor(
      steps = nrow(preference)
    )

    #computing preference order
    preference$score <- future.apply::future_lapply(
      X = preference$predictor,
      FUN = function(x) {
        p()

        out <- f.response(
          df = data.frame(
            y = df[[response]],
            x = df[[x]]
          ),
          function_name = function_name,
          cv_training_fraction = cv_training_fraction,
          cv_iterations = cv_iterations
        ) |>
          mean(na.rm = TRUE)

        if (is.na(out)) {
          warning(
            "\n",
            function_name,
            ": function '",
            attributes(f.response)$name,
            "' produced NA scores for response '",
            response,
            " and predictor '",
            x,
            "."
          )
        }

        out
      },
      future.seed = TRUE
    ) |>
      unlist() |>
      suppressWarnings()

    #reorder preference
    preference <- preference[
      order(
        preference$score,
        decreasing = TRUE
      ),
    ]

    rownames(preference) <- NULL

    #join metric if function is in f_functions()
    if (unique(preference[["f"]]) %in% unique(f_df[["f"]])) {
      preference <- merge(
        x = preference,
        y = f_df,
        by = "f"
      )
    } else {
      preference$metric <- "custom"
    }

    preference$rank <- seq_len(length.out = nrow(preference))

    out_list[[response]] <- preference
  } #end of loop

  out_df <- do.call(
    what = "rbind",
    args = out_list
  )

  rownames(out_df) <- NULL

  out_df <- out_df[, c("response", "predictor", "f", "metric", "score", "rank")]

  out_df$score <- round(
    x = out_df$score,
    digits = 4
  )

  out_df
}
