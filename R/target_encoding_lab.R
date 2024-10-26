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
#'   \item "mean" (implemented in `target_encoding_mean()`): Encodes categorical values with the group means of the response. Variables encoded with this method are identified with the suffix "__encoded_mean". It has a method to control overfitting implemented via the argument `smoothing`. The integer value of this argument indicates a threshold in number of rows. Groups above this threshold are encoded with the group mean, while groups below it are encoded with a weighted mean of the group's mean and the global mean. This method is named "mean smoothing" in the relevant literature.
#'   \item "rank" (implemented in `target_encoding_rank()`): Returns the rank of the group as a integer, being 1 he group with the lower mean of the response variable. Variables encoded with this method are identified with the suffix "__encoded_rank".
#'   \item "loo" (implemented in `target_encoding_loo()`): Known as the "leave-one-out method" in the literature, it encodes each categorical value with the mean of the response variable across all other group cases. This method controls overfitting better than "mean". Variables encoded with this method are identified with the suffix "__encoded_loo".
#' }
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams collinear
#' @param response (optional, character string) Name of a numeric response variable in `df`. Default: NULL.
#' @param methods (optional; character vector or NULL). Name of the target encoding methods. If NULL, target encoding is ignored, and `df` is returned with no modification. Default: c("loo", "mean", "rank")
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
      "loo",
      "mean",
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
    message("\ncollinear::target_encoding_lab(): argument 'quiet' must be logical, resetting it to FALSE.")
    quiet <- FALSE
  }

  # validate all other args ----
  args <- validate_encoding_arguments(
    df = df,
    response = response,
    predictors = predictors,
    methods = methods,
    smoothing = smoothing,
    white_noise = white_noise,
    seed = seed,
    overwrite = overwrite,
    quiet = quiet
  )

  #reassign args
  if(is.data.frame(args$df)){
    df <- args$df
  }
  response <- args$response
  predictors <- args$predictors
  methods <- args$methods
  smoothing <- args$smoothing
  white_noise <- args$white_noise
  seed <- args$seed
  overwrite <- args$overwrite

  # early stops ----
  if(
    is.null(response) ||
    length(predictors) == 0 ||
    is.null(methods) ||
    (
      !is.null(response) &&
      !is.numeric(df[[response]])
    )
  ){
    return(df)
  }

  #message to start encoding
  if(quiet == FALSE){

    message(
      "\ncollinear::target_encoding_lab(): using response '",
      response,
      "' to encode categorical predictors:\n - ",
      paste0(
        predictors,
        collapse = "\n - "
      )
    )

  }

  # target-encoding ----
  combinations.df <- expand.grid(
    predictor = predictors,
    white_noise = white_noise,
    smoothing = smoothing,
    seed = seed,
    method = methods,
    stringsAsFactors = FALSE
  )

  #progress bar
  p <- progressr::progressor(
    steps = nrow(combinations.df)
  )

  #original data frame names
  df_names <- colnames(df)

  #parallel encoding
  encoded_list <- future.apply::future_apply(
    X = combinations.df,
    MARGIN = 1,
    FUN = function(x){

      p()

      #testing vector
      # x <- c("biogeo_ecoregion", "0", "0", "1", "mean")
      # names(x) <- c("predictor", "white_noise", "smoothing", "seed", "method")

      #args
      smoothing.i <- as.integer(x["smoothing"])
      white_noise.i <- as.numeric(x["white_noise"])
      seed.i <- as.integer(x["seed"])
      method.i <- x[["method"]]
      predictor.i <- x["predictor"]

      #subset df
      df.i <- df[, c(response, predictor.i)]

      #to character
      df.i[[predictor.i]] <- as.character(df.i[[predictor.i]])

      #NA to "NA"
      df.i[[predictor.i]] <- replace(
        x = df.i[[predictor.i]],
        list = is.na(df.i[[predictor.i]]),
        values = "NA"
      )

      #new predictor name
      predictor_encoded_name.i <- encoded_predictor_name(
        predictor = predictor.i,
        encoding_method = method.i,
        smoothing = smoothing.i,
        white_noise = white_noise.i,
        seed = seed.i
      )

      #get encoding function
      f.i <- get(
        x = paste0(
          "target_encoding_",
          method.i
        )
      )

      #apply encoding function
      df.i <- f.i(
        df = df.i,
        response = response,
        predictor = predictor.i,
        encoded_name = predictor_encoded_name.i,
        smoothing = smoothing.i
      )

      #add white noise if any
      df.i <- add_white_noise(
        df = df.i,
        response = response,
        predictor = predictor_encoded_name.i,
        white_noise = white_noise.i,
        seed = seed.i
      )

      #replacing original variable with encoded version
      if(overwrite == TRUE){

        #remove original column
        df.i[[predictor.i]] <- NULL

        #rename encoded column
        colnames(df.i)[colnames(df.i) == predictor_encoded_name.i] <- predictor.i

      } else {

        predictor.i <- predictor_encoded_name.i

      }

      df.i[, predictor.i, drop = FALSE]

    }, #end of lambda function
    future.seed = TRUE
  ) #end of loop

  # encoded predictors ----
  encoded_df <- do.call(
    what = "cbind",
    args = encoded_list
  )

  #remove original predictors
  if(overwrite == TRUE){

    df <- df[, !(colnames(df) %in% predictors), drop = FALSE]

  }

  #merge encoded data
  df <- cbind(
    df,
    encoded_df
  )

  df

}


