#' Quantitative Variable Prioritization for Multicollinearity Filtering
#'
#' @description
#' Ranks predictors by the strength of their association with a response. Aims to minimize the loss of important predictors during multicollinearity filtering.
#'
#' The strength of association between the response and each predictor is computed by the function \code{f}. The function [f_auto()] can select a suitable one automatically depending on the data types of the response and the predictors. For more fine-tuned control, the package offers the following functions:
#'
#' \itemize{
#'   \item **Numeric response vs numeric predictor**:
#'   \itemize{
#'    \item [f_r2_pearson()]: Pearson's R-squared.
#'    \item [f_r2_spearman()]: Spearman's R-squared.
#'    \item [f_r2_glm_gaussian()]: Pearson's R-squared of response versus the predictions of a Gaussian GLM.
#'    \item [f_r2_glm_gaussian_poly2()]: Gaussian GLM with second degree polynomial.
#'    \item [f_r2_gam_gaussian()]: GAM model fitted with [mgcv::gam()].
#'    \item [f_r2_rpart()]: Recursive Partition Tree fitted with [rpart::rpart()].
#'    \item [f_r2_rf()]: Random Forest model fitted with [ranger::ranger()].
#'   }
#'   \item **Integer counts response vs. numeric predictor**:
#'   \itemize{
#'     \item [f_r2_glm_poisson()]: Pearson's R-squared of a Poisson GLM.
#'     \item [f_r2_glm_poisson_poly2()]: Poisson GLM with second degree polynomial.
#'     \item [f_r2_gam_poisson()]: Poisson GAM.
#'   }
#'   \item **Binomial response (1 and 0) vs. numeric predictor**:
#'   \itemize{
#'     \item [f_auc_glm_binomial()]: AUC of quasibinomial GLM with weighted cases.
#'     \item [f_auc_glm_binomial_poly2()]: As above with second degree polynomial.
#'     \item [f_auc_gam_binomial()]: Quasibinomial GAM with weighted cases.
#'     \item [f_auc_rpart()]: Recursive Partition Tree with weighted cases.
#'     \item [f_auc_rf()]: Random Forest model with weighted cases.
#'   }
#'   \item **Categorical response (character of factor) vs. categorical predictor**:
#'   \itemize{
#'     \item [f_v()]: Cramer's V between two categorical variables.
#'   }
#'   \item **Categorical response vs. categorical or numerical predictor**:
#'   \itemize{
#'     \item [f_v_rf_categorical()]: Cramer's V of a Random Forest model.
#'   }
#' }
#'
#' Additionally, any custom function accepting a data frame with the columns "x" (predictor) and "y" (response) and returning a numeric indicator of association where higher numbers indicate higher association will work.
#'
#' This function returns a data frame with the column "predictor", with predictor names ordered by the column "preference", with the result of \code{f}. This data frame, or the column "predictor" alone, can be used as inputs for the argument \code{preference_order} in [collinear()], [cor_select()], and [vif_select()].
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' Accepts a character vector of response variables as input for the argument \code{response}. When more than one response is provided, the output is a named list of preference data frames.
#'
#' @inheritParams collinear
#'
#' @param f (optional: function name) Unquoted name of the function to compute preference order. Available functions are listed in the column `name` of the dataframe returned by [f_functions()]. By default calls to [f_auto()] to select a suitable method depending on the nature of the data (see [f_auto_rules()]). Default: f_auto
#'
#' @param warn_limit (optional, numeric) Preference value (R-squared, AUC, or Cramer's V) over which a warning flagging suspicious predictors is issued. Disabled if NULL. Default: NULL
#' @family preference_order
#' @return data frame:
#' \itemize{
#'   \item \code{response}: character, response name.
#'   \item \code{predictor}: character, name of the predictor.
#'   \item \code{f}: name of the function used to compute the preference order.
#'   \item \code{preference}: value returned by  \code{f} to represent the association between the \code{response} and each given \code{predictor}
#'   \item \code{metric}: optional, name of the metric in \code{preference}, only returned when \code{f} is in [f_functions()].
#' }
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_categorical,
#'   vi_predictors_numeric
#' )
#'
#' ##OPTIONAL: parallelization setup
#' # future::plan(
#' #   future::multisession,
#' #   workers = 2
#' # )
#'
#' ##OPTIONAL: progress bar
#' ##does not work in R examples
#' #progressr::handlers(global = TRUE)
#'
#' #numeric response and predictors
#' #------------------------------------------------
#' #selects f automatically depending on data features
#' #applies f_r2_pearson() to compute correlation between response and predictors
#' #returns data frame ordered by preference
#' x <- preference_order(
#'   df = vi_smol,
#'   responses = c("vi_categorical", "vi_counts"),
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_auto
#'   )
#'
#' x
#'
#' #f_auto selects a different function and metric for each response
#' unique(x$f)
#' unique(x$metric)
#'
#'
#' #it can be plugged into collinear
#' y <- collinear(
#'   df = vi_smol,
#'   responses = c("vi_categorical", "vi_counts"),
#'   predictors = vi_predictors_numeric[1:10],
#'   preference_order = x
#' )
#'
#' #f function selected by user
#' #for binomial response and numeric predictors
#' x <- preference_order(
#'   df = vi,
#'   responses = "vi_binomial",
#'   predictors = vi_predictors_numeric[1:10],
#'   f = f_auc_glm_binomial
#' )
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
    warn_limit = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::preference_order()",
    ... = ...
  )

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  #check f
  f <- validate_arg_f(
    f = f,
    f_name = deparse(substitute(f)),
    function_name = function_name
  )

  if(is.null(f)){

    if(quiet == FALSE){
      message(
        "\n",
        function_name,
        ": argument 'f' is NULL, skipping computation of preference order."
      )
    }

    return(NULL)

  }

  #check input data frame
  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  #managing multiple responses
  responses <- intersect(
    x = colnames(df),
    y = responses
  )

  f_df <- f_functions()[, c("name", "preference_metric")]
  colnames(f_df) <- c("f", "metric")

  #output list
  out_list <- list()

  #iterating over responses
  for(response in responses){

    if(
      quiet == FALSE &&
      length(responses) > 1
    ){

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

    #check response
    response <- validate_arg_response(
      df = df,
      response = response,
      function_name = function_name,
      quiet = quiet
    )

    #check predictors
    predictors.response <- validate_arg_predictors(
      df = df,
      response = response,
      predictors = predictors,
      function_name = function_name,
      quiet = quiet
    )


    #use f_auto to get function name
    if(all(c("df", "response", "predictors", "quiet") %in% names(formals(f)))){

      #get function name
      f_name <- f(
        df = df,
        response = response,
        predictors = predictors.response,
        quiet = quiet
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

    #data frame to store results
    preference <- data.frame(
      response = rep(
        x = response,
        times = length(predictors.response)
        ),
      predictor = predictors.response,
      f = rep(
        x = attributes(f.response)$name,
        times = length(predictors.response)
      )
    )

    #progress bar
    p <- progressr::progressor(
      steps = nrow(preference)
    )

    #computing preference order
    preference$preference <- future.apply::future_lapply(
      X = preference$predictor,
      FUN = function(x){

        p()

        f.response(
          df = data.frame(
            y = df[[response]],
            x = df[[x]]
          ) |>
            stats::na.omit()
        )

      },
      future.seed = TRUE
    ) |>
      unlist() |>
      suppressWarnings()

    #reorder preference
    preference <- preference[
      order(
        preference$preference,
        decreasing = TRUE),
    ]

    rownames(preference) <- NULL

    #assess extreme associations
    if(
      is.numeric(warn_limit) &&
      quiet == FALSE
      ){

      preference_extreme <- preference[preference$preference > warn_limit, ]

      if(nrow(preference_extreme) > 0){

         message(
           "\n",
           function_name,
           ": predictors with associations to '",
           response,
           "' higher than ",
           warn_limit,
           ": \n - ",
           paste0(
             preference_extreme$predictor,
             ": ",
             round(
               preference_extreme$preference,
               2
               ),
             collapse = "\n - "
           )
         )

      }

    }

    #join metric
    if(unique(preference$f) %in% unique(f_df$f)){

      preference <- merge(
        x = preference,
        y = f_df,
        by = "f"
      )

    }

    attr(
      x = preference,
      which = "validated"
    ) <- TRUE

    out_list[[response]] <- preference

  } #end of loop

  out_df <- do.call(
    what = "rbind",
    args = out_list
  )

  rownames(out_df) <- NULL

  out_df <- out_df[, c("response", "predictor", "preference", "f", "metric")]

  out_df

}
