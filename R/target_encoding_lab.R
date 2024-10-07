#' Target Encoding Lab: Transform Categorical Variables to Numeric
#'
#' @description
#'
#' Target encoding involves replacing the values of categorical variables with numeric ones derived from a "target variable", usually a model's response.
#'
#' In essence, target encoding works as follows:
#' \itemize{
#'   \item 1. group all cases belonging to a unique value of the categorical variable.
#'   \item 2. compute a statistic of the target variable across the group cases.
#'   \item 3. assign the value of the statistic to the group.
#' }
#'
#' The methods to compute the group statistic implemented here are:
#'
#' \itemize{
#'   \item "mean" (implemented in `target_encoding_mean()`): Replaces categorical values with the group means of the numeric variable. It has two methods to control overfitting:
#'   \itemize{
#'      \item `smoothing` groups larger than this argument are encoded with the group mean, while smaller groups are encoded with a weighted mean of the group's and the global mean. This method is named "mean smoothing" in the relevant literature.
#'      \item `white_noise`: maximum white noise to be added to each case, expressed as a fraction of the observed range of the numeric variable. Non-deterministic, requires setting the `seed` argument for reproducibility.
#'   }
#'   Variables encoded with this method are identified with the suffix "__encoded_mean".
#'   \item "rank" (implemented in `target_encoding_rank()`): Returns the rank of the group as a integer, being 1 he group with the lower mean of the response variable. It accepts the `white_noise` argument to control overfitting. Variables encoded with this method are identified with the suffix "__encoded_rank".
#'   \item "loo" (implemented in `target_encoding_loo()`): Known as the "leave-one-out method" in the literature, it replaces each categorical value with the mean of the response variable across all other group cases. This method controls overfitting better than "mean". Additionally, it accepts the `white_noise` method. Variables encoded with this method are identified with the suffix "__encoded_loo".
#' }
#'
#'
#'
#' @inheritParams collinear
#' @param encoding_methods (optional; character vector or NULL). Name of the target encoding methods. If NULL, target encoding is ignored, and `df` is returned with no modification. Default: c("mean", "loo", rank")
#' @param smoothing (optional; integer) Argument of the method "mean". Groups smaller than this number have their means pulled towards mean of the response across all cases. Default: 0
#' @param white_noise (optional; numeric) Argument of the methods "mean", "rank", and "loo". Maximum white noise to add, expressed as a fraction of the range of the response variable. Default: `0`.
#' @param seed (optional; integer) Random seed to facilitate reproducibility when `white_noise` is not 0. Default: 1
#' @param replace (optional; logical) If `TRUE`, only the first method in `encoding_methods` is used, the method suffix is ignored, and the categorical variables are overwritten with their encoded versions in the output data frame. Default: FALSE
#' @param verbose (optional; logical) If TRUE, messages generated during the execution of the function are printed to the console Default: TRUE
#'
#' @return data frame
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
#'   response = "vi_numeric",
#'   predictors = "koppen_zone",
#'   encoding_methods = c(
#'     "mean",
#'     "loo",
#'     "rank"
#'   ),
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
#'   x = df[["vi_numeric"]],
#'   y = df[, predictors.encoded],
#'   use = "pairwise.complete.obs"
#' )
#'
#'
#' @autoglobal
#' @family target_encoding
#' @author Blas M. Benito, PhD
#' @references
#' \itemize{
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32. doi: 10.1145/507533.507538
#' }
#' @export
target_encoding_lab <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    encoding_methods = c(
      "mean",
      "loo",
      "rank"
    ),
    smoothing = 0,
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(is.null(encoding_methods)){
    return(df)
  }

  #testing method argument
  encoding_methods <- match.arg(
    arg = encoding_methods,
    choices = c(
      "mean",
      "loo",
      "rank"
    ),
    several.ok = TRUE
  )

  #validate input data frame
  df <- validate_df(
    df = df
  )

  #validate response
  response <- validate_response(
    df = df,
    response = response
  )

  if(is.null(response)){
    if(verbose == TRUE){
      message("collinear::target_encoding_lab(): argument 'response' is NULL, skipping target-encoding.")
    }
    return(df)
  }

  #return data frame if response is not numeric
    if(!is.numeric(df[[response]])){
      if(verbose == TRUE){
        message("collinear::target_encoding_lab(): argument 'response' is not numeric, skipping target-encoding.")
      }
      return(df)
    }

  #validate predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors
  )

  #if replace is true, get only first option of all inputs
  if(replace == TRUE){
    verbose <- FALSE
    encoding_methods <- encoding_methods[1]
    white_noise <- white_noise[1]
  }

  #return df if all predictors are numeric
  predictors.to.encode <- identify_predictors_categorical(
    df = df,
    predictors = predictors
  )

  if(length(predictors.to.encode) == 0){

    if(verbose == TRUE){
      message("collinear::target_encoding_lab(): all predictors are numeric, nothing to do.")
    }
    return(df)
  }

  if(verbose == TRUE){
    if(length(predictors.to.encode) > 1){
      message(
        "\n collinear::target_encoding_lab(): encoding the predictors:\n",
        paste0(
          predictors.to.encode,
          collapse = "\n"
        ),
        "\n"
      )
    } else {
      message(
        "\n collinear::target_encoding_lab(): encoding the predictor: ",
        predictors.to.encode,
        "\n"
      )
    }
  }

  #iterating over categorical variables
  for(predictors.to.encode.i in predictors.to.encode){

    for(white_noise.i in white_noise){

      #rank method
      if("rank" %in% encoding_methods){

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

      if("mean" %in% encoding_methods){

        for(smoothing.i in smoothing){

          df <- target_encoding_mean(
            df = df,
            response = response,
            predictor = predictors.to.encode.i,
            smoothing = smoothing.i,
            white_noise = white_noise.i,
            seed = seed,
            replace = replace,
            verbose = verbose
          )

        }

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

  } #end of iteration over predictors

  df

}


