#' @title Automated Multicollinearity Management
#'
#' @description
#'
#' Automates multicollinearity management in datasets with mixed variable types (numeric, categorical, and logical) through an integrated system of five key components:
#'
#' \itemize{
#'
#'   \item **Target Encoding Integration** (opt-in, disabled by default): When \code{responses} is numeric, categorical variables in \code{predictors} exist, and \code{encoding_method} is specified ("loo", "mean", or "rank"), categorical predictors are transparently converted to numeric using response values as reference. This enables VIF and correlation analysis across mixed data types. See [target_encoding_lab()] for implementation details.
#'
#'   \item **Intelligent Predictor Ranking** (active by default): Three prioritization strategies ensure the most relevant predictors are retained during filtering:
#'
#'   \itemize{
#'
#'     \item **User-defined ranking** (argument \code{preference_order}): Accepts a character vector of predictor names or a dataframe resulting from [preference_order()]. When collinear predictors are detected, those with lower ranking are removed. This option focuses the analysis on predictors of particular interest.
#'
#'     \item **Response-based ranking** (\code{f}): Uses functions like [f_auto], [f_numeric_glm], or [f_binomial_rf] to rank predictors by their association with the response. This functionality now supports cross-validation (see [preference_order()] for further details). Preserves predictors with stronger relationships to the response, yielding stronger predictive models. Requires valid \code{responses} and \code{preference_order = NULL}.
#'
#'     \item **Multicollinearity-based ranking** (default): When both \code{preference_order} and \code{f} are \code{NULL}, predictors are ranked from lower to higher multicollinearity. This preserves rare predictors over redundant ones, but might not result in robust models.
#'   }
#'
#'   \item **Unified Correlation Framework** (active by default): Computes pairwise correlations between any variable types using Pearson correlation (numeric-numeric), target encoding (numeric-categorical), and Cramer's V (categorical-categorical) within a single, consistent workflow. See [cor_df()], [cor_matrix()], and [cor_cramer()] for details.
#'
#'   \item **Adaptive Filtering Thresholds** (active by default): When both \code{max_cor} and \code{max_vif} are \code{NULL}, optimal thresholds are automatically determined based on the median correlation structure of the predictors (see "Adaptive Multicollinearity Thresholds" section).
#'
#'   \item **Dual Filtering Strategy** (active by default): Combines two complementary methods while respecting predictor rankings:
#'
#'   \itemize{
#'
#'     \item **Pairwise Correlation Filtering**: Removes predictors with correlation or Cramer's V above \code{max_cor}. Handles all variable type combinations. See [cor_select()] for the algorithm.
#'
#'     \item **VIF-based Filtering**: Removes numeric predictors with Variance Inflation Factors above \code{max_vif}. See [vif_select()], [vif_df()], and [vif()] for implementation details.
#'   }
#' }
#'
#' This function accepts parallelization via [future::plan()] and progress bars via [progressr::handlers()]. Parallelization benefits [target_encoding_lab()], [preference_order()], and [cor_select()] operations, particularly with categorical variables or large datasets.
#'
#' @section Adaptive Multicollinearity Thresholds:
#'
#' When both \code{max_cor} and \code{max_vif} are \code{NULL}, the function automatically determines optimal filtering thresholds based on the correlation structure of the predictors:
#'
#' \enumerate{
#'
#'   \item The median correlation between all predictors is computed via [cor_stats()].
#'
#'   \item The value of \code{max_cor} is set to the median correlation of the dataset.
#'
#'   \item The value of \code{max_vif} is computed from \code{max_cor} using the model [gam_cor_to_vif], which maps correlation thresholds to their equivalent VIF values.
#'
#' }
#'
#' Under this data-drive approach, datasets with lower median correlation receive stricter thresholds, while those with higher baseline correlation are filtered more permissively, preventing over-filtering of genuinely informative predictors.
#'
#' In validation experiments (see [experiment_adaptive_thresholds]), this automatic configuration method decreased median correlation by an average of -0.24, and achieved an average maximum VIF of 1.40 across selections. These results indicate this system is robust to an ample set of use cases.
#'
#' Users can override automatic configuration by explicitly setting either \code{max_cor} or \code{max_vif} (or both) to specific numeric values.
#'
#' @section Variance Inflation Factors:
#'
#' The Variance Inflation Factor of a given predictor \eqn{a} is computed as \eqn{1/(1-R2)}, where \eqn{R2} is the multiple R-squared of a multiple regression model fitted using \eqn{a} as response against all other predictors, as in  \eqn{a = b + c + ...}.
#'
#' The square root of the VIF of \eqn{a} is the factor by which the confidence interval of the estimate for \eqn{a} in the linear model \eqn{y = a + b + c + ...} is widened by multicollinearity in the predictors.
#'
#' The range of VIF values is (1, Inf]. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5 (most conservative), 5, and 10 (most permissive).
#'
#' @section VIF-based Filtering:
#'
#' This algorithm, implemented in [vif_select()], selects numeric predictors with a Variance Inflation Factor lower than \code{max_vif} while preserving those with a higher ranking in \code{preference_order}, if provided.
#'
#' The algorithm works as follows:
#'
#' \enumerate{
#'
#'   \item All numeric predictors are ranked according to the argument \code{preference_order}, or from lower to higher VIF with the other predictors (as computed by [vif_df()]) otherwise. Categorical predictors are ignored.
#'
#'   \item The predictor with the higher rank is added to the selection.
#'
#'   \item The VIF of the next predictor in the ranking against the predictor/s in the selection is computed via [cor_matrix()] and [vif()]. If the resulting VIF is lower than \code{max_vif}, the new predictor is added to the selection. Otherwise it is removed.
#'
#'   \item Step 3 is repeated until all predictors are processed.
#'
#' }
#'
#' This filtering method ensures that all predictors in the selection are correlated below \code{max_vif} and that the selection respects the prioritization defined in \code{preference_order}.
#'
#' @section Pairwise Correlation Filtering:
#'
#' This algorithm is implemented in [cor_select()], and selects numeric and categorical predictors with Pearson correlation or Cramer's V association lower than \code{max_cor} while preserving those with a higher ranking in \code{preference_order}, if provided.
#'
#' It works as follows:
#'
#' \enumerate{
#'
#'   \item The correlation matrix of all predictors is computed, its diagonals are set to zero, and the rows and columns are ordered according to \code{preference_order}, or from lower to higher sum of correlations with the other predictors (([colSums()] applied to the output of [cor_matrix()])) otherwise.
#'
#'   \item The predictor with the higher rank is added to the selection.
#'
#'   \item The correlation/s between the predictor/s in the selection and the next predictor in the ranking is/are extracted from the global correlation matrix. If the maximum correlation is below the value of \code{max_cor}, the new predictor is added to the selection. Otherwise it is removed.
#'
#'   \item Step 3 is repeated until all predictors are processed.
#'
#' }
#'
#' This filtering method ensures that all predictors in the selection are correlated below \code{max_cor} and that the selection respects the prioritization defined in \code{preference_order}.
#'
#'
#' @param df (required; dataframe, tibble, or sf) A dataframe with responses (optional) and predictors. Must have at least 10 rows for pairwise correlation analysis, and \code{10 * (length(predictors) - 1)} for VIF analysis.  Default: NULL.
#'
#' @param responses (optional; character, character vector, or NULL) Name of one or several response variables in \code{df}. Default: NULL.
#'
#' @param predictors (optional; character vector or NULL) Names of the predictors in \code{df}. If NULL, all columns in \code{df} except the ones in \code{responses} and those with constant values or near zero variance are used. Default: NULL
#'
#' @param encoding_method (optional; character or NULL). Name of one target encoding method. One of: "loo", "mean", or "rank" (see [target_encoding_lab()] for further details). If NULL, target encoding is disabled. Default: NULL
#'
#' @param preference_order (optional; character vector, dataframe resulting from [preference_order()], or NULL). Prioritizes predictors to preserve the most relevant ones during multicollinearity filtering. Overrides \code{f} when not NULL.
#'
#' @param f (optional: unquoted function name or NULL). Function to rank \code{predictors} depending on their relationship with the \code{responses}. Available functions are listed by [f_functions()] and described in the manual of [preference_order()]. Requires valid \code{responses} and \code{preference_order = NULL}. Default: `f_auto`
#'
#' @param max_cor (optional; numeric or NULL) Maximum correlation allowed between pairs of \code{predictors}. Valid values are between 0.01 and 0.99, and recommended values are between 0.5 (strict) and 0.9 (permissive). If NULL and \code{max_vif = NULL}, it is set to the median correlation of all predictors. Otherwise it is ignored. Default: NULL
#'
#' @param max_vif (optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2.5 (strict) and 10 (permissive). If NULL and \code{max_cor = NULL}, it is configured automatically to a VIF matching the median correlation of the predictors. Otherwise it is ignored. Default: NULL.
#'
#' @param quiet (optional; logical) If FALSE, messages are printed to the console. Default: FALSE
#'
#' @param ... (optional) Used to pass internal arguments such as \code{function_name} for [validate_arg_function_name] in nested calls, or \code{m}, a correlation matrix generated via [stats::cor()] or [cor_matrix()], and the cross-validation arguments of [preference_order()]: \code{cv_iterations}, and \code{cv_training_fraction}.
#'
#' @return list of class \code{collinear_output} with sub-lists of class \code{collinear_selection}.
#'
#' If \code{responses = NULL}, only one sub-list named "result" is produced. Otherwise, a sub-list named after each response is generated. A sub-list of class \code{collinear_selection} contains:
#' \itemize{
#'   \item \code{response} (character): name of the response, if any.
#'   \item \code{df} (dataframe):  input data with the given \code{response}, if any, and the selected \code{predictors}. If target encoding was triggered, then categorical predictors will show as numeric in this dataframe.
#'   \item{preference_order} (dataframe): dataframe generated by [preference_order()].
#'   \item \code{selection} (character vector): names of the selected \code{predictors}.
#'   \item \code{formulas} (list): if \code{responses} is not NULL, list with modelling formulas generated by [model_formula()] on the given response and the selected predictors. When the response and the predictors are numeric, the formulas "linear" and "smooth" (for GAM models) are generated. When the response is categorical, a "classification" formula is returned.
#' }
#'
#' @examples
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
#' #progressr::handlers(global = TRUE)
#'
#' ##minimal setup
#' #--------------------------
#' # no response
#' # all columns in df are filtered
#' # uses numeric columns only to speed-up example
#' x <- collinear(
#'   df = vi_smol[, vi_predictors_numeric]
#' )
#'
#' #same as above
#' # x <- collinear(
#' #   df = vi_smol,
#' #   predictors = vi_predictors_numeric
#' # )
#'
#' #results are contained in a list of class "collinear_output"
#' class(x)
#'
#' #this class has a print() method
#' print(x) #same as typing x without print()
#'
#' #and a summary method that returns the selected variables
#' y <- summary(x)
#' str(y)
#' y$result
#'
#' #when response = NULL
#' #x contains one sublist named "result"
#' #of class "collinear_selection"
#' names(x)
#' class(x$result)
#'
#' #sublists also have print and summary methods
#' print(x$result)
#' y <- summary(x$result)
#' y
#'
#' #using responses and predictors
#' #--------------------------
#' # - numeric responses and predictors to speed-up example
#' # - ordered from lower to higher collinearity
#' x <- collinear(
#'   df = vi_smol,
#'   responses = c("vi_numeric", "vi_categorical"),
#'   predictors = vi_predictors_numeric,
#'   max_cor = 0.7,
#'   max_vif = 5,
#'   f = NULL,
#'   quiet = TRUE
#' )
#'
#' #when responses is used
#' #sub-lists are named after the response
#' names(x)
#'
#' x$vi_categorical
#' x$vi_numeric
#'
#' #selections are identical because f = NULL
#' x$vi_categorical$selection
#' x$vi_numeric$selection
#'
#' #when f = NULL, predictors are ranked
#' #from lower to higher multicollinearity
#' x$vi_categorical$preference_order$df
#'
#' #the output also contains model formulas
#' x$vi_categorical$formulas$classification
#' x$vi_numeric$formulas$linear #lm formula
#' x$vi_numeric$formulas$smooth #basic GAM formula
#'
#' #the dataframes with the response
#' #and the selected predictors are also provided
#' colnames(x$vi_categorical$df)
#' colnames(x$vi_numeric$df)
#'
#' #so we can fit a model right away
#' m <- lm(
#'   formula = x$vi_numeric$formulas$linear,
#'   data = x$vi_numeric$df
#' )
#'
#' summary(m)
#'
#' #manual preference order
#' #---------------------------
#' x <- collinear(
#'   df = vi_smol,
#'   responses = "vi_numeric",
#'   predictors = vi_predictors_numeric,
#'   preference_order = c(
#'     "swi_mean",
#'     "soil_temperature_mean",
#'     "growing_season_length",
#'     "rainfall_mean"
#'   ),
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' #first predictor in preference order is always selected
#' #other predictors in preference order are included
#' #depending on their correlation with the first one
#' x$vi_numeric$selection[1] == "swi_mean"
#'
#' #predictors not in preference_order are ranked from lower to higher multicollinearity
#' head(x$vi_numeric$preference_order$df)
#'
#'
#' #automatic preference order
#' #--------------------------
#' #f = f_auto selects an adequate method for each response
#' x <- collinear(
#'   df = vi_smol,
#'   responses = c("vi_numeric", "vi_categorical"),
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_auto,
#'   max_cor = 0.7,
#'   max_vif = 5
#' )
#'
#' #each response has a different preference order
#' head(x$vi_categorical$preference_order$df$predictor)
#' head(x$vi_numeric$preference_order$df$predictor)
#'
#' #filtering results are slightly different now
#' x$vi_categorical$selection
#' x$vi_numeric$selection
#'
#' #resetting to sequential processing
#' #future::plan(future::sequential)
#'
#' @autoglobal
#' @references
#' \itemize{
#'  \item David A. Belsley, D.A., Kuh, E., Welsch, R.E. (1980). Regression Diagnostics: Identifying Influential Data and Sources of Collinearity. John Wiley & Sons. DOI: 10.1002/0471725153.
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32. DOI: 10.1145/507533.507538
#' }
#' @family automated_multicollinearity_analysis
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
){

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
  if(ncol(df) < df.ncol){

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
  if(
    !is.null(encoding_method) &&
    !is.null(responses) &&
    any(responses %in% var.types$numeric) &&
    any(predictors %in% var.types$categorical)
    ){

    target_encoding_needed <- TRUE

    if(length(responses) == 1){

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
  if(
    is.null(max_cor) &&
    is.null(max_vif)
    ){

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

    cor.stats <- cor_stats(
      df = cor.df,
      predictors = predictors,
      quiet = TRUE,
      function_name = function_name
    )

    max_cor <- cor.stats[
      cor.stats$statistic == "median",
      "value"
    ]

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": setting 'max_cor' to ",
        max_cor,
        "."
      )

    }

    max_vif <- mgcv::predict.gam(
      object = collinear::gam_cor_to_vif,
      newdata = data.frame(
        max_cor = max_cor
      )
    ) |>
      round(digits = 4)

    if(quiet == FALSE){

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
  if(
    target_encoding_needed == FALSE &&
    is.null(m)
    ){

    m <- cor_matrix(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  #manage response for loop
  if(
    length(responses) == 0 ||
    is.null(responses)
  ){
    responses <- list(NULL)
  }

  #if several responses, invalidate predictors
  if(
    length(responses) > 1 &&
    any(responses %in% predictors)
    ){
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
  for(response in responses){

    if(
      quiet == FALSE &&
      length(responses) > 1
    ){

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
    if(
      !is.null(m) &&
      length(predictors.response) < length(colnames(m))
    ){

      m <- m[
        predictors.response,
        predictors.response
        ]

      class(m) <- unique(c("collinear_cor_matrix", class(m)))

    }

    ## TARGET ENCODING ----
    if(
      target_encoding_needed &&
      response %in% var.types$numeric &&
      any(predictors.response %in% var.types$categorical)
    ){

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

    if(
      is.null(preference_order) &&
      !is.null(response) &&
      !is.null(f)
    ){


      if(is.null(dots$cv_training_fraction)){
        dots$cv_training_fraction <- 1
      }

      if(is.null(dots$cv_iterations)){
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

    df.response <- df.response[
      ,
      c(response, selection.response),
      drop = FALSE
    ]

    attr(
      x = df.response,
      which = "validated"
    ) <- TRUE

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": selected predictors: \n - ",
        paste(selection.response, collapse = "\n - ")
      )

    }

    #prepare output list
    out.response <- list()

    if(!is.null(response)){
      out.response$response <- response
    }

    out.response$df <- df.response
    out.response$preference_order <- preference_order.response
    out.response$selection <- selection.response

    if(
      all(
        c(
          !is.null(response),
          !is.null(selection.response)
        )
      )
    ){

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
      if(
        response_type == "numeric" &&
        length(selection_type$numeric) > 0
      ){

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
    if(is.null(response)){
      slot_name <- "result"
    }

    out[[slot_name]] <- out.response

  } #end of loop

  class(out) <- c("collinear_output", class(out))

  out

}
