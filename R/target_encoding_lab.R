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
#'   \item "mean" (implemented in `target_encoding_mean()`): Encodes categorical values with the group means of the response. It has two methods to control overfitting:
#'   \itemize{
#'      \item `smoothing` groups larger than this argument are encoded with the group mean, while smaller groups are encoded with a weighted mean of the group's and the global mean. This method is named "mean smoothing" in the relevant literature.
#'      \item `white_noise`: maximum white noise to be added to each case, expressed as a fraction of the observed range of the numeric variable. Non-deterministic, requires setting the `seed` argument for reproducibility.
#'   }
#'   Variables encoded with this method are identified with the suffix "__encoded_mean".
#'   \item "rank" (implemented in `target_encoding_rank()`): Returns the rank of the group as a integer, being 1 he group with the lower mean of the response variable. It accepts the `white_noise` argument to control overfitting. Variables encoded with this method are identified with the suffix "__encoded_rank".
#'   \item "loo" (implemented in `target_encoding_loo()`): Known as the "leave-one-out method" in the literature, it encodes each categorical value with the mean of the response variable across all other group cases. This method controls overfitting better than "mean". Additionally, it accepts the `white_noise` method. Variables encoded with this method are identified with the suffix "__encoded_loo".
#' }
#'
#'
#'
#' @inheritParams collinear
#' @param methods (optional; character vector or NULL). Name of the target encoding methods. If NULL, target encoding is ignored, and `df` is returned with no modification. Default: c("mean", "loo", rank")
#' @param smoothing (optional; integer vector) Argument of the method "mean". Groups smaller than this number have their means pulled towards the mean of the response across all cases. Default: 0
#' @param white_noise (optional; numeric vector) Argument of the methods "mean", "rank", and "loo". Maximum white noise to add, expressed as a fraction of the range of the response variable. Range from 0 to 1. Default: `0`.
#' @param seed (optional; integer vector) Random seed to facilitate reproducibility when `white_noise` is not 0. If NULL, the function selects one at random, and the selected seed does not appear in the encoded variable names. Default: 0
#' @param overwrite (optional; logical) If `TRUE`, the original predictors in `df` are overwritten with their encoded versions, but only one encoding method, smoothing, white noise, and seed are allowed. Otherwise, encoded predictors with their descriptive names are added to `df`. Default: FALSE
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
#'   methods = c(
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
    methods = c(
      "mean",
      "loo",
      "rank"
    ),
    smoothing = 0,
    white_noise = 0,
    seed = 0,
    overwrite = FALSE,
    quiet = FALSE
){

  # quiet ----
  if(!is.logical(quiet)){
    message("collinear::target_encoding_lab(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  # early stops ----
  if(is.null(methods)){

    if(quiet == FALSE){

      message(
        "collinear::target_encoding_lab(): argument 'encoding_method' is NULL, skipping target encoding."
      )

    }

    return(df)

  }

  # validate df ----
  df <- validate_df(
    df = df,
    quiet = quiet
  )

  # validate predictors
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

  # numeric args ----

  ## white noise ----
  white_noise <- as.numeric(white_noise)

  white_noise <- white_noise[white_noise >= 0 & white_noise <= 1]

  if(length(white_noise) == 0){
    white_noise <- 0
  }

  ## smoothing ----
  smoothing <- as.integer(smoothing)

  smoothing <- smoothing[smoothing >= 0 & smoothing <= nrow(df)]

  if(length(smoothing) == 0){
    smoothing <- 0
  }

  ## seed ----
  if(!is.null(seed)){
    seed_ <- as.integer(seed)
  } else {
    seed_ <- sample.int(
      x = .Machine$integer.max,
      size = 1
      )
  }

  # methods ----
  valid_methods <- c(
    "mean",
    "loo",
    "rank"
  )

  methods <- intersect(
    x = methods,
    y = valid_methods
  )

  if(length(methods) == 0){

    if(quiet == FALSE){

      message(
        "collinear::target_encoding_lab(): argument 'methods' not valid, resetting it to default values."
      )

    }

    methods <- valid_methods

  }

  # overwrite ----
  if(!is.logical(overwrite) == FALSE){

    if(quiet == FALSE){
      message("collinear::target_encoding_lab(): argument 'overwrite' must be logical, resetting it to FALSE.")
    }

    overwrite <- FALSE

  }

  if(overwrite == TRUE){

    if(length(methods) > 1){

      if(quiet == FALSE){

        message(
          "collinear::target_encoding_lab(): only one encoding method allowed when 'overwrite = TRUE', using method: '",
          methods[1], "'."
        )

      }

      methods <- methods[1]

    }

    if(length(white_noise) > 1){

      if(quiet == FALSE){

        message("collinear::target_encoding_lab(): only one 'white_noise' value allowed when 'overwrite = TRUE', using value: ", white_noise[1], ".")

      }

      white_noise <- white_noise[1]

    }

    if(length(smoothing) > 1){

      if(quiet == FALSE){

        message("collinear::target_encoding_lab(): only one 'smoothing' value allowed when 'overwrite = TRUE', using value: ", smoothing[1], ".")

      }

      smoothing <- smoothing[1]

    }

    if(length(seed_) > 1){

      if(quiet == FALSE){

        message("collinear::target_encoding_lab(): only one 'seed' value allowed when 'overwrite = TRUE', using value: ", seed_[1], ".")

      }

      seed_ <- seed_[1]

    }

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
    method = methods,
    stringsAsFactors = FALSE
  )

  #add ID to DF
  df$id.. <- seq_len(nrow(df))

  #original data frame names
  df_names <- colnames(df)

  #TODO: review and simplify this logic if possible

  #parallel encoding
  encoded_list <- future.apply::future_apply(
    X = combinations.df,
    MARGIN = 1,
    FUN = function(x){

      #testing vector
      # x <- c("biogeo_ecoregion", "0", "0", "1", "mean")
      # names(x) <- c("predictor", "white_noise", "smoothing", "seed", "method")

      f <- get(
        x = paste0(
          "target_encoding_",
          x[["method"]]
        )
      )

      #encode df
      df.i <- f(
        df = df,
        response = response,
        predictor = x["predictor"],
        smoothing = as.integer(x["smoothing"]),
        white_noise = as.numeric(x["white_noise"]),
        seed = as.integer(x["seed"]),
        overwrite = overwrite,
        quiet = quiet
      )

      if(overwrite == FALSE){

        df_new_column <- setdiff(
          x = colnames(df.i),
          y = df_names
        )

        df.i <- df.i[, c("id..", df_new_column)]

      } else {

        df.i <- df.i[, c("id..", x["predictor"]), drop = FALSE]

      }

      #arrange by id
      df.i[
        order(
          abs(df.i$id..)
        ),
        , drop = FALSE
      ]

    }
  )

  #merge by ID
  encoded_df <- Reduce(
    f = function(x, y){merge(x, y, by = "id..")},
    x = encoded_list
    )

  #remove original predictors
  if(overwrite == TRUE){

    df <- df[, !(colnames(df) %in% predictors), drop = FALSE]

  }

  #merge encoded data
  df <- merge(
    x = df,
    y = encoded_df,
    by = "id.."
  )

  #remove id
  df$id.. <- NULL

  df

}


