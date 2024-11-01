#' Quantitative Variable Prioritization for Multicollinearity Filtering
#'
#' @description
#' Ranks a set of predictors by the strength of their association with a response. Aims to minimize the loss of important predictors during multicollinearity filtering.
#'
#' The strength of association between the response and each predictor is computed by the function `f`. The `f` functions available are:
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
#' The name of the used function is stored in the attribute "f_name" of the output data frame. It can be retrieved via `attributes(df)$f_name`
#'
#' Additionally, any custom function accepting a data frame with the columns "x" (predictor) and "y" (response) and returning a numeric indicator of association where higher numbers indicate higher association will work.
#'
#' This function returns a data frame with the column "predictor", with predictor names ordered by the column "preference", with the result of `f`. This data frame, or the column "predictor" alone, can be used as inputs for the argument `preference_order` in [collinear()], [cor_select()], and [vif_select()].
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' Accepts a character vector of response variables as input for the argument `response`. When more than one response is provided, the output is a named list of preference data frames.
#'
#' @inheritParams collinear
#' @param warn_limit (optional, numeric) Preference value (R-squared, AUC, or Cramer's V) over which a warning flagging suspicious predictors is issued. Disabled if NULL. Default: NULL
#' @family preference_order
#' @return data frame: columns are "response", "predictor", "f" (function name), and "preference".
#' @examples
#' #subsets to limit example run time
#' df <- vi[1:1000, ]
#' predictors <- vi_predictors[1:10]
#' predictors_numeric <- vi_predictors_numeric[1:10]
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
#' #numeric response and predictors
#' #------------------------------------------------
#' #selects f automatically depending on data features
#' #applies f_r2_pearson() to compute correlation between response and predictors
#' df_preference <- preference_order(
#'   df = df,
#'   response = "vi_numeric",
#'   predictors = predictors_numeric,
#'   f = NULL
#'   )
#'
#' #returns data frame ordered by preference
#' df_preference
#'
#'
#' #several responses
#' #------------------------------------------------
#' responses <- c(
#'   "vi_categorical",
#'   "vi_counts"
#' )
#'
#' preference_list <- preference_order(
#'   df = df,
#'   response = responses,
#'   predictors = predictors
#' )
#'
#' #returns a named list
#' names(preference_list)
#' preference_list[[1]]
#' preference_list[[2]]
#'
#' #can be used in collinear()
#' # x <- collinear(
#' #   df = df,
#' #   response = responses,
#' #   predictors = predictors,
#' #   preference_order = preference_list
#' # )
#'
#' #f function selected by user
#' #for binomial response and numeric predictors
#' # preference_order(
#' #   df = vi,
#' #   response = "vi_binomial",
#' #   predictors = predictors_numeric,
#' #   f = f_auc_glm_binomial
#' # )
#'
#'
#' #disable parallelization
#' future::plan(future::sequential)
#' @autoglobal
#' @author Blas M. Benito, PhD
#' @export
preference_order <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    f = "auto",
    warn_limit = NULL,
    quiet = FALSE
){

  if(!is.logical(quiet)){
    message("\ncollinear::preference_order(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  if(is.null(response)){

    if(quiet == FALSE){

      message("\ncollinear::preference_order(): argument 'response' is NULL, skipping computation of preference order.")

    }

    return(NULL)
  }

  #check input data frame
  df <- validate_df(
    df = df,
    quiet = quiet
  )

  #managing multiple responses
  responses <- intersect(
    x = colnames(df),
    y = response
  )
  rm(response)

  #copy of predictors
  predictors_user <- predictors

  #copy of f
  f_user <- f
  f_user_name <- deparse(substitute(f))

  #output list
  out <- list()

  #iterating over responses
  for(response in responses){

    #check response
    response <- validate_response(
      df = df,
      response = response,
      quiet = quiet
    )

    if(quiet == FALSE){

      message("\ncollinear::preference_order(): ranking predictors for response '", response, "'." )

    }

    #check predictors
    predictors.response <- validate_predictors(
      df = df,
      response = response,
      predictors = predictors_user,
      quiet = quiet
    )

    #select f function
    if(is.null(f_user) ||
       (
         is.character(f_user) &&
        f_user == "auto"
        )
       ){

      #get function name
      f_name <- f_auto(
        df = df,
        response = response,
        predictors = predictors.response,
        quiet = quiet
      )

      #as function
      f <- get(f_name)

    } else {

      f <- f_user
      f_name <- f_user_name

    }

    #data frame to store results
    preference <- data.frame(
      response = rep(
        x = response,
        times = length(predictors.response)
        ),
      predictor = predictors.response,
      f = rep(
        x = f_name,
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

        f(
          df = data.frame(
            y = df[[response]],
            x = df[[x]]
          ) |>
            na.omit()
        )

      }, #end of lambda function
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
           "\ncollinear::preference_order(): [WARNING] predictors with associations to '",
           response,
           "' higher than ",
           warn_limit,
           ": \n - ",
           paste0(
             preference_extreme$predictor,
             ": ",
             round(preference_extreme$preference, 2),
             collapse = "\n - "
           )
         )

      }

    }

    out[[response]] <- preference

  } #end of loop


  if(is.list(out) && length(out) == 1){
    out <- out[[1]]
  }

  out

}
