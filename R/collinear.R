#' @title Smart multicollinearity management
#'
#' @description
#'
#' Automates multicollinearity management in datasets with mixed variable types
#' (numeric, categorical, and logical) through an integrated system of five key
#' components:
#'
#' \describe{
#'   \item{Target Encoding Integration (opt-in)}{
#'     When \code{responses} is numeric, categorical predictors can be converted
#'     to numeric using response values as reference. This enables VIF and
#'     correlation analysis across mixed types. See \code{\link{target_encoding_lab}}.
#'   }
#'
#'   \item{Intelligent Predictor Ranking (active by default)}{
#'     Three prioritization strategies ensure the most relevant predictors are
#'     retained during filtering:
#'     \itemize{
#'       \item \strong{User-defined ranking} (argument \code{preference_order}):
#'         Accepts a character vector of predictor names or a dataframe from
#'         \code{\link{preference_order}}. Lower-ranked collinear predictors are removed.
#'       \item \strong{Response-based ranking} (\code{f}):
#'         Uses \code{\link{f_auto}}, \code{\link{f_numeric_glm}}, or
#'         \code{\link{f_binomial_rf}} to rank predictors by association with
#'         the response. Supports cross-validation via \code{\link{preference_order}}.
#'       \item \strong{Multicollinearity-based ranking} (default):
#'         When both \code{preference_order} and \code{f} are \code{NULL},
#'         predictors are ranked from lower to higher multicollinearity.
#'     }
#'   }
#'
#'   \item{Unified Correlation Framework (active by default)}{
#'     Computes pairwise correlations between variable types using Pearson
#'     (numeric–numeric), target encoding (numeric–categorical), and Cramer's V
#'     (categorical–categorical). See \code{\link{cor_df}}, \code{\link{cor_matrix}},
#'     and \code{\link{cor_cramer}}.
#'   }
#'
#'   \item{Adaptive Filtering Thresholds (active by default)}{
#'     When \code{max_cor} and \code{max_vif} are both \code{NULL}, thresholds
#'     are determined from the median correlation structure of the predictors.
#'   }
#'
#'   \item{Dual Filtering Strategy (active by default)}{
#'     Combines two complementary methods while respecting predictor rankings:
#'     \itemize{
#'       \item \strong{Pairwise Correlation Filtering}:
#'         Removes predictors with Pearson correlation or Cramer's V above
#'         \code{max_cor}. See \code{\link{cor_select}}.
#'       \item \strong{VIF-based Filtering}:
#'         Removes numeric predictors with VIF above \code{max_vif}. See
#'         \code{\link{vif_select}}, \code{\link{vif_df}}, and \code{\link{vif}}.
#'     }
#'   }
#' }
#'
#' This function accepts parallelization via \code{future::plan()} and progress
#' bars via \code{progressr::handlers()}. Parallelization benefits
#' \code{\link{target_encoding_lab}}, \code{\link{preference_order}}, and
#' \code{\link{cor_select}}.
#'
#' @section Adaptive Multicollinearity Thresholds:
#'
#' When both \code{max_cor} and \code{max_vif} are \code{NULL}, the function
#' determines thresholds as follows:
#' \enumerate{
#'   \item Compute the 75th percentile of pairwise correlations via
#'     \code{\link{cor_stats}}.
#'   \item Map that value through a sigmoid between 0.545 (VIF~2.5) and 0.785
#'     (VIF~7.5), centered at 0.665, to get \code{max_cor}.
#'   \item Compute \code{max_vif} from \code{max_cor} using
#'     \code{\link{gam_cor_to_vif}}.
#' }
#'
#' @section Variance Inflation Factors:
#'
#' VIF for predictor \eqn{a} is computed as \eqn{1/(1-R^2)}, where \eqn{R^2} is
#' the multiple R-squared from regressing \eqn{a} on the other predictors.
#' Recommended maximums commonly used are 2.5, 5, and 10.
#'
#' @section VIF-based Filtering:
#'
#' \code{\link{vif_select}} ranks numeric predictors (user \code{preference_order}
#' if provided, otherwise from lower to higher VIF) and sequentially adds
#' predictors whose VIF against the current selection is below \code{max_vif}.
#'
#' @section Pairwise Correlation Filtering:
#'
#' \code{\link{cor_select}} computes the global correlation matrix, orders
#' predictors by \code{preference_order} or by lower-to-higher summed
#' correlations, and sequentially selects predictors with pairwise correlations
#' below \code{max_cor}.
#'
#' @param df (required; dataframe, tibble, or sf) A dataframe with responses
#'   (optional) and predictors. Must have at least 10 rows for pairwise
#'   correlation analysis, and \code{10 * (length(predictors) - 1)} for VIF.
#'   Default: NULL.
#'
#' @param responses (optional; character, character vector, or NULL) Name of
#'   one or several response variables in \code{df}. Default: NULL.
#'
#' @param predictors (optional; character vector or NULL) Names of the
#'   predictors in \code{df}. If NULL, all columns except \code{responses} and
#'   constant/near-zero-variance columns are used. Default: NULL.
#'
#' @param encoding_method (optional; character or NULL) One of "loo", "mean",
#'   or "rank". If NULL, target encoding is disabled. Default: NULL.
#'
#' @param preference_order (optional; character vector, dataframe from
#'   \code{\link{preference_order}}, or NULL) Prioritizes predictors to preserve.
#'
#' @param f (optional; unquoted function name or NULL) Function to rank
#'   predictors by relationship with \code{responses}. See \code{\link{f_functions}}.
#'   Default: \code{f_auto}.
#'
#' @param max_cor (optional; numeric or NULL) Maximum allowed pairwise
#'   correlation (0.01–0.99). Recommended between 0.5 and 0.9. If NULL and
#'   \code{max_vif} is NULL, it is selected automatically. Default: NULL.
#'
#' @param max_vif (optional; numeric or NULL) Maximum allowed VIF. Recommended
#'   between 2.5 and 10. If NULL and \code{max_cor} is NULL, configured
#'   automatically. Default: NULL.
#'
#' @param quiet (optional; logical) If FALSE, messages are printed. Default: FALSE.
#'
#' @param ... (optional) Internal args (e.g. \code{function_name} for
#'   \code{\link{validate_arg_function_name}}, a precomputed correlation matrix
#'   \code{m}, or cross-validation args for \code{\link{preference_order}}).
#'
#' @return A list of class \code{collinear_output} with sublists of class
#'   \code{collinear_selection}. If \code{responses = NULL} a single sublist
#'   named "result" is returned; otherwise a sublist per response is returned.
#'
#' @examples
#' data(vi_smol, vi_predictors_numeric)
#' x <- collinear(df = vi_smol[, vi_predictors_numeric])
#'
#' @autoglobal
#' @references
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression
#'    Diagnostics: Identifying Influential Data and Sources of Collinearity.
#'    John Wiley & Sons. DOI: 10.1002/0471725153.
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality
#'    Categorical Attributes in Classification and Prediction Problems. SIGKDD
#'    Explor. Newsl. 3, 1, 27-32. DOI: 10.1145/507533.507538
#' }
#' @family multicollinearity_filtering
#' @export
collinear <- function(
  df = NULL,
  responses = NULL,
  predictors = NULL,
  encoding_method = NULL,
  preference_order = NULL,
  f = f_auto,
  max_cor = NULL,
  max_vif = NULL,
  quiet = FALSE,
  ...
) {
  dots <- list(...)

  function_name <- validate_arg_function_name(
    default_name = "collinear::collinear()",
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

  responses <- validate_arg_responses(
    df = df,
    responses = responses,
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
    responses = responses,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #revalidate predictors if columns were removed
  if (ncol(df) < df.ncol) {
    attributes(responses)$validated <- NULL
    attributes(predictors)$validated <- NULL

    responses <- validate_arg_responses(
      df = df,
      responses = responses,
      quiet = quiet,
      function_name = function_name
    )

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )
  }

  #identify variable types
  var.types <- identify_valid_variables(
    df = df,
    predictors = c(responses, predictors),
    quiet = quiet,
    function_name = function_name
  )

  #check if target encoding will be triggered
  target_encoding_needed <- FALSE
  if (
    !is.null(encoding_method) &&
      !is.null(responses) &&
      any(responses %in% var.types$numeric) &&
      any(predictors %in% var.types$categorical)
  ) {
    target_encoding_needed <- TRUE

    if (length(responses) == 1) {
      df <- target_encoding_lab(
        df = df,
        response = responses,
        predictors = predictors,
        encoding_method = encoding_method,
        overwrite = TRUE,
        quiet = quiet,
        function_name = function_name
      )

      target_encoding_needed <- FALSE
    }
  }

  #setup max cor and max vif if NULL
  m <- dots$m
  if (
    is.null(max_cor) &&
      is.null(max_vif)
  ) {
    #computing cor df and m here to avoid recomputation in collinear()
    cor.df <- cor_df(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

    m <- cor_matrix(
      df = cor.df,
      quiet = quiet,
      function_name = function_name
    )

    collinearity.stats <- collinear_stats(
      df = cor.df,
      predictors = predictors,
      quiet = TRUE,
      function_name = function_name
    )

    #check if filtering is needed at all
    vif.max <- collinearity.stats[
      collinearity.stats$method == "vif" &
        collinearity.stats$statistic == "maximum",
      "value"
    ]

    cor.max <- collinearity.stats[
      collinearity.stats$method == "correlation" &
        collinearity.stats$statistic == "maximum",
      "value"
    ]

    #data needs no filtering
    if (vif.max <= 2.5) {
      max_vif <- 2.5000
      max_cor <- round(x = cor.max, digits = 4)
    } else {
      #adaptive max_cor threshold using sigmoid soft clamping
      #smooth transition between conservative and permissive filtering

      #correlation structure of the input data
      cor.statistic <- collinearity.stats[
        collinearity.stats$method == "correlation" &
          collinearity.stats$statistic == "quantile_0.75",
        "value"
      ]

      #max_cor = 0.545 corresponds to VIF ~ 2.5 (conservative)
      prediction_df <- collinear::prediction_cor_to_vif

      sigmoid_floor <- prediction_df[
        which.min(abs(prediction_df$max_vif - 2.5)),
        "max_cor"
      ]

      #max_cor = 0.785 corresponds to VIF ~ 7.5 (permissive)
      sigmoid_ceiling <- prediction_df[
        which.min(abs(prediction_df$max_vif - 7.5)),
        "max_cor"
      ]

      #sigmoid params
      sigmoid_midpoint <- (sigmoid_floor + sigmoid_ceiling) / 2
      sigmoid_range <- sigmoid_ceiling - sigmoid_floor

      #smooth sigmoid transition between floor and ceiling
      #parameter (-15) controls transition sharpness
      max_cor <- sigmoid_floor +
        sigmoid_range /
          (1 + exp(-15 * (cor.statistic - sigmoid_midpoint)))

      max_cor <- round(x = max_cor, digits = 4)

      max_vif <- mgcv::predict.gam(
        object = collinear::gam_cor_to_vif,
        newdata = data.frame(
          max_cor = max_cor
        )
      ) |>
        round(digits = 4)
    }

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": setting 'max_cor' to ",
        max_cor,
        "."
      )

      message(
        "\n",
        function_name,
        ": setting 'max_vif' to ",
        max_vif,
        "."
      )
    }
  }

  #compute correlation matrix if needed
  if (
    target_encoding_needed == FALSE &&
      is.null(m)
  ) {
    m <- cor_matrix(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )
  }

  #manage response for loop
  if (
    length(responses) == 0 ||
      is.null(responses)
  ) {
    responses <- list(NULL)
  }

  #if several responses, invalidate predictors
  if (
    length(responses) > 1 &&
      any(responses %in% predictors)
  ) {
    attributes(predictors)$validated <- NULL
  }

  #parse f's name
  f <- validate_arg_f(
    f = f,
    f_name = deparse(substitute(f)),
    function_name = function_name
  )

  #ITERATION ----
  out <- list()

  ## start ----
  for (response in responses) {
    if (
      quiet == FALSE &&
        length(responses) > 1
    ) {
      msg <- paste0(
        function_name,
        ": processing response '",
        response,
        "'"
      )

      message("\n", msg)
      message(rep(x = "-", times = nchar(msg)))
    }

    ## copy df ----
    df.response <- df

    ## validate predictors ----
    predictors.response <- validate_arg_predictors(
      df = df.response,
      responses = response,
      predictors = predictors,
      function_name = function_name
    )

    ## subset correlation matrix to the given predictors
    if (
      !is.null(m) &&
        length(predictors.response) < length(colnames(m))
    ) {
      m <- m[
        predictors.response,
        predictors.response
      ]

      class(m) <- unique(c("collinear_cor_matrix", class(m)))
    }

    ## TARGET ENCODING ----
    if (
      target_encoding_needed &&
        response %in% var.types$numeric &&
        any(predictors.response %in% var.types$categorical)
    ) {
      df.response <- target_encoding_lab(
        df = df.response,
        response = response,
        predictors = predictors.response,
        encoding_method = encoding_method,
        overwrite = TRUE,
        quiet = quiet,
        function_name = function_name
      )
    }

    ## PREFERENCE ORDER ----
    preference_order.response <- preference_order

    if (
      is.null(preference_order) &&
        !is.null(response) &&
        !is.null(f)
    ) {
      if (is.null(dots$cv_training_fraction)) {
        dots$cv_training_fraction <- 1
      }

      if (is.null(dots$cv_iterations)) {
        dots$cv_iterations <- 1
      }

      preference_order.response <- preference_order(
        df = df.response,
        responses = response,
        predictors = predictors.response,
        f = f,
        cv_training_fraction = dots$cv_training_fraction,
        cv_iterations = dots$cv_iterations,
        quiet = quiet,
        function_name = function_name,
        m = m
      )
    }

    #validated here so we can put it in the output list
    preference_order.response <- validate_arg_preference_order(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = preference_order.response,
      quiet = quiet,
      function_name = function_name,
      m = m
    )

    ##MULTICOLLINEARITY ANALYSIS ----
    selection.response <- collinear_select(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = preference_order.response,
      max_cor = max_cor,
      max_vif = max_vif,
      quiet = quiet,
      function_name = function_name,
      m = m
    )

    ##PREPARE OUTPUT ----

    df.response <- df.response[,
      c(response, selection.response),
      drop = FALSE
    ]

    attr(
      x = df.response,
      which = "validated"
    ) <- TRUE

    if (quiet == FALSE) {
      message(
        "\n",
        function_name,
        ": selected predictors: \n - ",
        paste(selection.response, collapse = "\n - ")
      )
    }

    #prepare output list
    out.response <- list()

    if (!is.null(response)) {
      out.response$response <- response
    }

    out.response$df <- df.response
    out.response$preference_order <- preference_order.response
    out.response$selection <- selection.response

    if (
      all(
        c(
          !is.null(response),
          !is.null(selection.response)
        )
      )
    ) {
      response_type <- identify_valid_variables(
        df = df.response,
        predictors = response,
        function_name = function_name
      ) |>
        unlist() |>
        names()

      selection_type <- identify_valid_variables(
        df = df.response,
        predictors = selection.response,
        function_name = function_name
      )

      out.response$formulas <- list()

      general_formula <- model_formula(
        df = df.response,
        response = response,
        predictors = selection.response,
        quiet = quiet,
        function_name = function_name
      )

      general_formula_name <- c(
        numeric = "linear",
        logical = "binomial",
        categorical = "classification"
      )[[response_type]]

      out.response$formulas[[general_formula_name]] <- general_formula

      #add smooth formula if relevant
      if (
        response_type == "numeric" &&
          length(selection_type$numeric) > 0
      ) {
        out.response$formulas[["smooth"]] <- model_formula(
          df = df.response,
          response = response,
          predictors = selection.response,
          term_f = "s",
          quiet = quiet,
          function_name = function_name
        )
      }
    }

    class(out.response) <- c(
      "collinear_selection",
      class(out.response)
    )

    slot_name <- response
    if (is.null(response)) {
      slot_name <- "result"
    }

    out[[slot_name]] <- out.response
  } #end of loop

  class(out) <- c("collinear_output", class(out))

  out
}
