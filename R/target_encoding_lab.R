#' Target Encoding Lab: Transform Categorical Variables to Numeric
#'
#' @description
#'
#' Target encoding maps the values of categorical variables (of class \code{character} or \code{factor}) to numeric using another numeric variable as reference. The encoding methods implemented here are:
#'
#' \itemize{
#'   \item "mean" (implemented in [target_encoding_mean()]): Maps each category to the average of reference numeric variable across the category cases. Variables encoded with this method are identified with the suffix "__encoded_mean". It has a method to control overfitting implemented via the argument \code{smoothing}. The integer value of this argument indicates a threshold in number of rows. Categories sized above this threshold are encoded with the group mean, while groups below it are encoded with a weighted mean of the group's mean and the global mean. This method is named "mean smoothing" in the relevant literature.
#'   \item "rank" (implemented in [target_encoding_rank()]): Returns the rank of the group as a integer, being 1 he group with the lower mean of the reference variable. Variables encoded with this method are identified with the suffix "__encoded_rank".
#'   \item "loo" (implemented in [target_encoding_loo()]): Known as the "leave-one-out method" in the literature, it encodes each categorical value with the mean of the response variable across all other group cases. This method controls overfitting better than "mean". Variables encoded with this method are identified with the suffix "__encoded_loo".
#' }
#'
#' Accepts a parallelization setup via [future::plan()] and a progress bar via [progressr::handlers()] (see examples).
#'
#' @inheritParams collinear
#'
#' @param response (optional, character string) Name of a numeric response variable in \code{df}. Default: NULL.
#'
#' @param encoding_method (optional; character vector or NULL). Name of the target encoding methods. One or several of: "mean", "rank", "loo". If NULL, target encoding is ignored, and \code{df} is returned with no modification. Default: c("loo", "mean", "rank")
#'
#' @param smoothing (optional; integer vector) Argument of the method "mean". Groups smaller than this number have their means pulled towards the mean of the response across all cases. Default: 0
#'
#' @param overwrite (optional; logical) If TRUE, the original predictors in \code{df} are overwritten with their encoded versions, but only one encoding method, smoothing, white noise, and seed are allowed. Otherwise, encoded predictors with their descriptive names are added to \code{df}. Default: FALSE
#'
#' @inheritParams collinear
#'
#' @return dataframe
#' @examples
#'
#' data(vi_smol)
#'
#' #applying all methods for a continuous response
#' df <- target_encoding_lab(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = "koppen_zone",
#'   encoding_method = c(
#'     "mean",
#'     "loo",
#'     "rank"
#'   )
#' )
#'
#' #identify encoded predictors
#' predictors.encoded <- grep(
#'   pattern = "*__encoded*",
#'   x = colnames(df),
#'   value = TRUE
#' )
#'
#' head(df[, predictors.encoded])
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
    encoding_method = c(
      "loo",
      "mean",
      "rank"
    ),
    smoothing = 0,
    overwrite = FALSE,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::target_encoding_lab()",
    ... = ...
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  # early stops ----

  ## df is NULL ----
  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  # encoding method
  encoding_method <- validate_arg_encoding_method(
    encoding_method = encoding_method,
    overwrite = overwrite,
    quiet = quiet,
    function_name = function_name
  )

  if(is.null(encoding_method)){
    return(df)
  }

  response <- validate_arg_responses(
    df = df,
    responses = response,
    max_responses = 1,
    quiet = quiet,
    function_name = function_name
  )

  if(is.null(response)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' is NULL, skipping target encoding."
      )

    }

    return(df)

  }

  predictors <- validate_arg_predictors(
    df = df,
    responses = response,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  df <- validate_arg_df(
    df = df,
    responses = response,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )


  #check that response is numeric
  response_type <- identify_response_type(
    df = df,
    response = response,
    quiet = quiet,
    function_name = function_name
  )

  if(response_type %in% c("categorical", "unknown")){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": 'response' column '", response, "' is not numeric, skipping target encoding."
      )

    }

    return(df)

  }

  ## predictors ----
  predictors <- identify_categorical_variables(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )$valid

  if(length(predictors) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": no categorical predictors in argument 'predictors', skipping target encoding.")

    }

    return(df)

  }

  # overwrite ----
  if(is.logical(overwrite) == FALSE){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'overwrite' must be logical, resetting it to 'FALSE'."
      )

    }

    overwrite <- FALSE

  }


  ## smoothing ----
  if("mean" %in% encoding_method){

    smoothing <- as.integer(smoothing)

    smoothing <- smoothing[smoothing >= 0 & smoothing <= nrow(df)]

    if(length(smoothing) == 0){

      smoothing <- 0

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": invalid values in argument 'smoothing', resetting it to '",
          smoothing,
          "'."
        )

      }

    }

    #depends on overwrite
    if(
      overwrite == TRUE &&
      length(smoothing) > 1
    ){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one 'smoothing' value allowed when 'overwrite = TRUE', resetting it to '",
          smoothing[1],
          "'."
        )

      }

      smoothing <- smoothing[1]

    }

  }


  #message to start encoding
  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": using response '",
      response,
      "' to encode these categorical predictors:\n - ",
      paste0(
        predictors,
        collapse = "\n - "
      )
    )

  }

  # target-encoding ----
  combinations.df <- expand.grid(
    predictor = predictors,
    smoothing = smoothing,
    encoding_method = encoding_method,
    stringsAsFactors = FALSE
  )

  #progress bar
  p <- progressr::progressor(
    steps = nrow(combinations.df)
  )

  #original dataframe names
  df_names <- colnames(df)

  #parallel encoding
  encoded_list <- future.apply::future_apply(
    X = combinations.df,
    MARGIN = 1,
    FUN = function(x){

      p()

      #testing vector
      # x <- c("biogeo_ecoregion", "0", "1", "mean")
      # names(x) <- c("predictor", "smoothing", "seed", "method")

      #args
      smoothing.i <- as.integer(x["smoothing"])
      encoding_method.i <- x["encoding_method"]
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
      #name of smoothing method
      #only for "mean" and smoothing != 0
      name.smoothing <- ifelse(
        test = encoding_method.i == "mean" && smoothing.i != 0,
        yes = paste0("__smoothing_", smoothing.i),
        no = ""
      )

      #predictor name
      predictor_encoded_name.i <- paste0(
        predictor.i,
        "__encoded_",
        encoding_method.i,
        name.smoothing
      )

      #get encoding function
      f.i <- switch(
        encoding_method.i,
        loo  = target_encoding_loo,
        mean = target_encoding_mean,
        rank = target_encoding_rank
      )

      #apply encoding function
      df.i <- f.i(
        df = df.i,
        response = response,
        predictor = predictor.i,
        encoded_name = predictor_encoded_name.i,
        smoothing = smoothing.i,
        function_name = function_name
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

  #add validated tag
  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}


