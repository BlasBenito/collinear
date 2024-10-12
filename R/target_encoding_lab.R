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
#' @param smoothing (optional; integer vector) Argument of the method "mean". Groups smaller than this number have their means pulled towards the mean of the response across all cases. Default: 0
#' @param white_noise (optional; numeric vector) Argument of the methods "mean", "rank", and "loo". Maximum white noise to add, expressed as a fraction of the range of the response variable. Default: `0`.
#' @param seed (optional; integer vector) Random seed to facilitate reproducibility when `white_noise` is not 0. Default: 1
#' @param replace (optional; logical) If `TRUE`, only the first method in `encoding_methods` is used, the method suffix is ignored, and the categorical variables are overwritten with their encoded versions in the output data frame. Default: FALSE
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
#'   y = df[, predictors.encoded, drop = FALSE],
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
    quiet = FALSE
){

  # quiet
  if(!is.logical(quiet)){
    message("collinear::target_encoding_lab(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  # encoding_methods ----
  if(is.null(encoding_methods)){

    if(quiet == FALSE){

      message(
        "collinear::target_encoding_lab(): argument 'encoding_method' is NULL, skipping target encoding."
      )

    }

    return(df)

  }

  methods <- c(
    "mean",
    "loo",
    "rank"
  )

  encoding_methods <- intersect(
    x = encoding_methods,
    y = methods
  )

  if(length(encoding_methods) == 0){

    if(quiet == FALSE){

      message(
        "collinear::target_encoding_lab(): argument 'encoding_methods' not valid, resetting it to 'mean'."
      )

    }

    encoding_methods <- "mean"

  }

  # replace ----
  if(!is.logical(replace) == FALSE){

    if(quiet == FALSE){
      message("collinear::target_encoding_lab(): argument 'replace' must be logical, resetting it to FALSE.")
    }

    replace <- FALSE

  }

  if(replace == TRUE){

    if(length(encoding_methods) > 1){

      if(quiet == FALSE){

        message(
          "collinear::target_encoding_lab(): only one encoding method allowed when 'replace = TRUE', using method: '",
          encoding_methods[1], "'."
        )

      }

      encoding_methods <- encoding_methods[1]

    }

    if(length(white_noise) > 1){

      if(quiet == FALSE){

        message("collinear::target_encoding_lab(): only one 'white_noise' value allowed when 'replace = TRUE', using value: ", white_noise[1], ".")

      }

      white_noise <- white_noise[1]

    }

    if(length(smoothing) > 1){

      if(quiet == FALSE){

        message("collinear::target_encoding_lab(): only one 'smoothing' value allowed when 'replace = TRUE', using value: ", smoothing[1], ".")

      }

      smoothing <- smoothing[1]

    }

  }

  # validate df ----
  df <- validate_df(
    df = df,
    quiet = quiet
  )

  # validate response ----
  response <- validate_response(
    df = df,
    response = response,
    quiet = quiet
  )

  if(is.null(response)){

    if(quiet == FALSE){

      message("collinear::target_encoding_lab(): argument 'response' is NULL, skipping target-encoding.")

    }

    return(df)

  }

  #return data frame if response is not numeric
  if(!is.numeric(df[[response]])){

    if(quiet == FALSE){

      message("collinear::target_encoding_lab(): argument 'response' is not numeric, skipping target-encoding.")

    }

    return(df)

  }

  # validate predictors ----
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors,
    quiet = quiet
  )

  #identify categorical predictors
  predictors <- identify_predictors_categorical(
    df = df,
    predictors = predictors
  )

  if(length(predictors) == 0){

    if(quiet == FALSE){
      message("collinear::target_encoding_lab(): no categorical predictors available, skipping target encoding.")
    }
    return(df)
  }

  if(quiet == FALSE){

    message(
      "\n collinear::target_encoding_lab(): encoding categorical predictors:\n - ",
      paste0(
        predictors,
        collapse = "\n - "
      )
    )

  }

  #combinations of arguments
  combinations.df <- expand.grid(
    predictor = predictors,
    white_noise = white_noise,
    smoothing = smoothing,
    seed = seed,
    method = encoding_methods,
    stringsAsFactors = FALSE
  )

  #TODO: seems to produce duplicated columns, check the issue!
  for(i in seq_len(nrow(combinations.df))){

    #get current row
    args <- combinations.df[i, ]

    #get encoding function
    f <- get(
      x = paste0(
        "target_encoding_",
        args[["method"]]
        )
      )

    #encode df
    df <- f(
      df = df,
      response = response,
      predictor = args[["predictor"]],
      smoothing = args[["smoothing"]],
      white_noise = args[["white_noise"]],
      seed = args[["seed"]],
      replace = replace,
      quiet = quiet
    )

  }

  df

}


