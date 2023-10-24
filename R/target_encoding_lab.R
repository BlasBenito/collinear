#' Target encoding of non-numeric variables
#'
#' @description
#'
#' Target encoding involves replacing the values of categorical variables with numeric ones from a "target variable", usually a model's response. Target encoding can be useful for improving the performance of machine learning models.
#'
#' This function identifies categorical variables in the input data frame, and transforms them using a set of target-encoding methods selected by the user, and returns the input data frame with the newly encoded variables.
#'
#' The target encoding methods implemented in this function are:
#'
#' \itemize{
#'   \item "rank": Returns the rank of the group as a integer, starting with 1 as the rank of the group with the lower mean of the response variable. The variables returned by this method are named with the suffix "__encoded_rank". This method is implemented in the function [target_encoding_rank()].
#'   \item "mean": Replaces each value of the categorical variable with the mean of the response across the category the given value belongs to. This option accepts the argument "white_noise" to limit potential overfitting. The variables returned by this method are named with the suffix "__encoded_mean". This method is implemented in the function [target_encoding_mean()].
#'   \item "rnorm": Computes the mean and standard deviation of the response for each group of the categorical variable, and uses [rnorm()] to generate random values from a normal distribution with these parameters. The argument `rnorm_sd_multiplier` is used as a multiplier of the standard deviation to control the range of values produced by [rnorm()] for each group of the categorical predictor. The variables returned by this method are named with the suffix "__encoded_rnorm".  This method is implemented in the function [target_encoding_rnorm()].
#'   \item "loo": This is the leave-one-out method, that replaces each categorical value with the mean of the response variable across the other cases within the same group. This method supports the `white_noise` argument to increase limit potential overfitting. The variables returned by this method are named with the suffix "__encoded_loo". This method is implemented in the function [target_encoding_loo()].
#' }
#'
#' The methods "mean" and "rank" support the `white_noise` argument, which is a fraction of the range of the `response` variable, and the maximum possible value of white noise to be added. For example, if `response` is within 0 and 1, a `white_noise` of 0.25 will add to every value of the encoded variable a random number selected from a normal distribution between -0.25 and 0.25. This argument helps control potential overfitting induced by the encoded variable.
#'
#' The method "rnorm" has the argument `rnorm_sd_multiplier`, which multiplies the standard deviation argument of the `\link[stats]{rnorm}` function to control the spread of the encoded values between groups. Values smaller than 1 reduce the spread in the results, while values larger than 1 have the opposite effect.
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: NULL
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: NULL
#' @param predictors (required; character vector) Names of all the predictors in `df`. Only character and factor predictors are processed, but all are returned in the "df" slot of the function's output.  Default: NULL
#' @param encoding_methods (optional; character string or vector). Name of the target encoding methods. Default: c("mean", "rank", "loo", "rnorm")
#' @param seed (optional; integer) Random seed to facilitate reproducibility when `white_noise` is not 0. Default: 1
#' @param white_noise (optional; numeric) Numeric with white noise values in the range 0-1, representing a fraction of the range of the response to be added as noise to the encoded variable. Controls the variability in the encoded variables to mitigate potential overfitting. Default: `0`.
#' @param rnorm_sd_multiplier (optional; numeric) Numeric with multiplier of the standard deviation of each group in the categorical variable, in the range 0-1. Controls the variability in the encoded variables to mitigate potential overfitting. Default: `1`
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
#'     \item `correlation_with_response`: R-squared resulting from [cor.test()] on the target-encoded variable and the response.
#'   }
#' }
#'
#' If the option `replace` is `TRUE`, then the input data frame is returned with the categorical variables replaced with their encoded versions.
#'
#'
#' @examples
#'
#loading example data
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #applying all methods for a continuous response
#' df <- target_encoding_lab(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = "koppen_zone",
#'   encoding_methods = c(
#'     "mean",
#'     "rank",
#'     "rnorm",
#'     "loo"
#'   ),
#'   rnorm_sd_multiplier = c(0, 0.1, 0.2),
#'   white_noise = c(0, 0.1, 0.2)
#' )
#'
#' #identify encoded predictors
#' predictors.encoded <- grep(
#'   pattern = "*__encoded*",
#'   x = colnames(df),
#'   value = TRUE
#' )
#'
#' #correlation between encoded predictors and the response
#' stats::cor(
#'   x = df[["vi_mean"]],
#'   y = df[, predictors.encoded],
#'   use = "pairwise.complete.obs"
#' )
#'
#'
#' @autoglobal
#' @author Blas M. Benito
#' @references
#' \itemize{
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32 \doi{10.1145/507533.507538}
#' }
#' @export
target_encoding_lab <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    encoding_methods = c(
      "mean",
      "rank",
      "loo",
      "rnorm"
    ),
    rnorm_sd_multiplier = 0,
    seed = 1,
    white_noise = 0,
    replace = FALSE,
    verbose = TRUE
){

  #testing method argument
  encoding_methods <- match.arg(
    arg = encoding_methods,
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
    response = response,
    predictors = predictors,
    min_numerics = 0
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
  )

  if(is.null(response)){
    if(verbose == TRUE){
      message("Argument 'response' is required for target encoding, but was not provided. Returning 'df' with no target-encoding.")
    }
    return(df)

  }

  #if replace is true, get only first option of all inputs
  if(replace == TRUE){
    verbose <- FALSE
    encoding_methods <- encoding_methods[1]
    white_noise <- white_noise[1]
    rnorm_sd_multiplier <- rnorm_sd_multiplier[1]
  }

  #return data if all predictors are numeric
  predictors.to.encode <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.to.encode) == 0){

    if(verbose == TRUE){
      message("All predictors are numeric, nothing to do.")
    }

    return(df)

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

    if("rank" %in% encoding_methods){

      df <- target_encoding_rank(
        df = df,
        response = response,
        predictor = predictors.to.encode.i,
        seed = seed,
        replace = replace,
        verbose = verbose
      )

    }

    for(white_noise.i in white_noise){

      #method "mean"
      if("mean" %in% encoding_methods){

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

      if("loo" %in% encoding_methods){

        df <- target_encoding_loo(
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


    if("rnorm" %in% encoding_methods){

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

  } #end of iteration over predictors

  df

}


