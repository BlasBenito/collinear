#' Target encoding of non-numeric variables
#'
#' @description
#'
#' Target encoding involves replacing the values of categorical variables with numeric ones from a "target variable", usually a model's response. Target encoding can be useful for improving the performance of machine learning models.
#'
#' This function identifies categorical variables in the input data frame, transforms them using a set of target-encoding methods defined by the user, and returns the input data frame with the newly encoded variables.
#'
#' The target encoding methods implemented in this function are:
#'
#' \itemize{
#'   \item `rank`: Returns the rank of the group as a integer, starting with 1 as the rank of the group with the lower mean of the response variable. This method accepts the  `white_noise` argument, which adds white noise to the result to increase data variability and reduce overfitting. The variables returned by this method are named with the suffix "__encoded_rank". This method is implemented in the function [target_encoding_rank()].
#'   \item `mean`: Returns the mean of the response for each group in the categorical variable. This option accepts `white_noise` to increase data variability. The variables returned by this method are named with the suffix "__encoded_mean".  This method is implemented in the function [target_encoding_mean()].
#'   \item `rnorm`: Computes the mean and standard deviation of the response for each group of the categorical variable, and uses [rnorm()] to generate values taken from a normal distribution with these parameters. The argument `rnorm_sd_multiplier` is used as a multiplier of the standard deviation to reduce the range of values produced by [rnorm()] for each group of the categorical predictor. The variables returned by this method are named with the suffix "__encoded_rnorm".  This method is implemented in the function [target_encoding_rnorm()].
#'   \item `loo`: This is the leave-one-out method. Each categorical value is replaced with the mean of the response variable across the other cases within the same group. The variables returned by this method are named with the suffix "__encoded_loo". This method is implemented in the function [target_encoding_loo()].
#' }
#'
#' The methods "mean" and "rank" support the `white_noise` argument. Values larger than zero in this argument add white noise to the target-encoded variables using `stats::rnorm()` via the function [target_encoding_white_noise()]). The `white_noise` argument represents a fraction of the average differences between groups of the target-encoded variable. For example, if white_noise = 0.25 and the target-encoded variable has the unique values c(1, 2, 3), as it could be the case when using the "rank" method, then the average between-groups difference would be 1, and the range of the white noise added to each row would go between 0 and 0.25
#'
#' The method "rnorm" has the argument `rnorm_sd_multiplier`, which multiplies the standard deviation argument of the `rnorm()` function to limit the spread of the encoded values between groups.
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: `NULL`
#' @param predictors (required; character vector) Names of all the predictors in `df`. Only character and factor predictors are processed, but all are returned in the "df" slot of the function's output.  Default: `NULL`
#' @param methods (optional; character string). Name of the target encoding methods. Default: `c("mean", "rank", "loo", "rnorm")`
#' @param seed (optional; integer) Random seed to facilitate reproducibility when `white_noise` is not 0. Default: 1
#' @param white_noise (optional; numeric vector) Numeric vector with white noise values in the range 0-1. Used only in methods "mean" and "rank". Generates white noise to reduce overfitting. Default: 0.
#' @param rnorm_sd_multiplier (optional; numeric vector) Only for the method "rnorm". Numeric vector with multiplicators of the standard deviation of each group in the categorical variable, in the range 0.01-1. Default: 0.1
#' @param replace (optional; logical) If `TRUE`, the function replaces each categorical variable with its encoded version, and returns the input data frame with the encoded variables instead of the original ones. Default: FALSE
#' @param verbose (optional; logical) If TRUE, messages generated during the execution of the function are printed to the console Default: TRUE
#'
#' @return
#' If no target encoding is needed because all predictors are numeric, the function returns `df`.
#'
#' Otherwise it returns a list with these slots:
#' \itemize{
#'   \item `df`: Input data frame, but with target-encoded character or factor columns.
#'   \item `correlation_test`: Data frame with the results of a linear model between the target-encoded variable and the response. It contains the following columns:
#'   \itemize{
#'     \item `encoded_predictor`: name of the target-encoded variable.
#'     \item `correlation_with_response`: R-squared resulting from `cor.test()` on the target-encoded variable and the response.
#'   }
#' }
#'
#' If the option `replace` is `TRUE`, then the input data frame is returned with the categorical variables replaced with their encoded versions.
#'
#'
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #applying all methods for a continuous response
#' output <- target_encoding_lab(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   methods = c(
#'     "mean",
#'     "rank",
#'     "rnorm",
#'     "loo"
#'   ),
#'   rnorm_sd_multiplier = c(0.01, 0.1, 1),
#'   white_noise = c(0, 1)
#' )
#'
#' #the output has several objects
#' names(output)
#'
#' #names of the encoded predictors
#' output$encoded_predictors
#'
#' #the data with the original and the encoded predictors
#' colnames(output$data)
#'
#' #a correlation test assessing the correlation between the response and the encoded predictors
#' output$leakage_test
#'
#' #plotting the transformations of "primary_productivity"
#' tidyr::pivot_longer(
#'   data = output$data,
#'   cols = dplyr::all_of(
#'     grep(
#'       pattern = "primary_productivity",
#'       x = output$encoded_predictors,
#'       value = TRUE)
#'     )
#' ) |>
#'   dplyr::select(
#'     plant_richness,
#'     primary_productivity,
#'     name,
#'     value
#'   ) |>
#'   ggplot2::ggplot() +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = value,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::facet_wrap(~name, scales = "free_y") +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(
#'     x = "Response values",
#'     y = "Encoded values",
#'     color = "Original\ngroups"
#'   )
#'
#' }
#' @autoglobal
#' @export
target_encoding_lab <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    methods = c(
      "mean",
      "rank",
      "loo",
      "rnorm"
      ),
    rnorm_sd_multiplier = 0.1,
    seed = 1,
    white_noise = 0,
    replace = FALSE,
    verbose = TRUE
){

  #testing method argument
  methods <- match.arg(
    arg = methods,
    choices = c(
      "mean",
      "rank",
      "loo",
      "rnorm"
    ),
    several.ok = TRUE
  )

  #validate input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #validate predictors
  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
  )

  #if replace is true, get only first option of all inputs
  if(replace == TRUE){
    verbose <- FALSE
    methods <- methods[1]
    white_noise <- white_noise[1]
    rnorm_sd_multiplier <- rnorm_sd_multiplier[1]
  }

  #factors to characters
  df <- rapply(
    object = df,
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  #return data if all predictors are numeric
  predictors.to.encode <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.to.encode) == 0){

    if(verbose == TRUE){
      message("All predictors are numeric, returning input data frame.")
    }

    return(data)

  }

  if(verbose == TRUE){
    message(
      "Encoding the variables:\n",
      paste0(
        predictors.to.encode,
        collapse = "\n"
      ),
      "\n"
    )
  }

  #iterating over categorical variables
  for(predictors.to.encode.i in predictors.to.encode){

      #method "mean"
      if("mean" %in% methods){

        for(white_noise.i in white_noise){

        df <- target_encoding_mean(
          df = df,
          response = response,
          predictor = predictors.to.encode.i,
          white_noise = white_noise.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

        }

      }

      if("rank" %in% methods){

        for(white_noise.i in white_noise){

        df <- target_encoding_rank(
          df = df,
          response = response,
          predictor = predictors.to.encode.i,
          white_noise = white_noise.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

        }

      }


    if("rnorm" %in% methods){

      for(rnorm_sd_multiplier.i in rnorm_sd_multiplier){

        df <- target_encoding_rnorm(
          df = df,
          response = response,
          predictor = predictors.to.encode.i,
          rnorm_sd_multiplier = rnorm_sd_multiplier.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

      }

    }

    if("loo" %in% methods){

      df <- target_encoding_loo(
        df = df,
        response = response,
        predictor = predictors.to.encode.i,
        replace = replace,
        verbose = verbose
      )

    }

  } #end of iteration over predictors

  df

}


