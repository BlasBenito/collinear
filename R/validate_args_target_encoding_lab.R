#' Validates Arguments of \code{target_encoding_lab()}
#'
#' @description
#' Internal function to validate configuration arguments for [target_encoding_lab()].
#'
#'
#' @inheritParams target_encoding_lab
#' @return list
#' @export
#' @autoglobal
#' @family data_validation
#' @examples
#' validate_args_target_encoding_lab(
#'   df = vi,
#'   response = "vi_numeric",
#'   predictors = vi_predictors
#'   )
validate_args_target_encoding_lab <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    encoding_method = c(
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

  function_name <- "collinear::target_encoding_lab()"

  # early stops ----

  ## df is NULL ----
  df <- validate_arg_df(
    df = df,
    response = response,
    predictors = predictors,
    function_name = function_name
  )

  ## response is NULL ----
  if(is.null(response)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' is NULL, skipping target encoding."
      )

    }

    return(NULL)

  }

  #response is categorical
  response_type <- identify_response_type(
    df = df,
    response = response
  )

  if(response_type == "categorical"){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' is categorical, skipping target encoding."
      )

    }

    return(NULL)

  }

  # overwrite ----
  if(is.logical(overwrite) == FALSE){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'overwrite' must be logical, resetting it to FALSE."
      )

    }

    overwrite <- FALSE

  }

  encoding_method <- validate_arg_encoding_method(
    encoding_method = encoding_method,
    overwrite = overwrite,
    function_name = function_name,
    quiet = quiet
  )

  ## encoding_method is NULL ----
  if(is.null(encoding_method)){
    return(NULL)
  }

  ## df ----
  df <- validate_arg_df(
    df = df,
    response = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## response ----
  response <- validate_arg_response(
    df = df,
    response = response,
    function_name = function_name,
    quiet = quiet
  )

  #check that response is numeric
  response <- identify_predictors_numeric(
    df = df,
    predictors = response
  )

  if(length(response) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'response' is not numeric, skipping target-encoding."
      )

    }

    return(NULL)

  }

  ## predictors ----
  predictors <- validate_arg_predictors(
    df = df,
    response = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  #identify categorical predictors
  predictors <- identify_predictors_categorical(
    df = df,
    predictors = predictors
  )

  if(length(predictors) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": no categorical predictors in argument 'predictors', skipping target encoding.")

    }

    return(NULL)

  }


  ## white noise ----
  if(is.null(white_noise)){
    white_noise <- 0
  }

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
    seed <- as.integer(seed)
  } else {
    seed <- sample.int(
      n = .Machine$integer.max,
      size = 1
    )
  }

  #depends on overwrite
  if(overwrite == TRUE){

    if(length(white_noise) > 1){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one 'white_noise' value allowed when 'overwrite = TRUE', using value: ",
          white_noise[1],
          "."
        )

      }

      white_noise <- white_noise[1]

    }

    if(length(smoothing) > 1){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one 'smoothing' value allowed when 'overwrite = TRUE', using value: ",
          smoothing[1],
          "."
        )

      }

      smoothing <- smoothing[1]

    }

    if(length(seed) > 1){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one 'seed' value allowed when 'overwrite = TRUE', using value: ",
          seed[1],
          "."
        )

      }

      seed <- seed[1]

    }

  }

  out <- list(
    df = df,
    response = response,
    predictors = predictors,
    encoding_method = encoding_method,
    smoothing = smoothing,
    white_noise = white_noise,
    seed = seed,
    overwrite = overwrite
  )

  out

}
