#' Validate Argument df
#'
#' @description
#' Internal function to validate the argument `df` and ensure it complies with the requirements of the package functions. It performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item Stops if 'df' cannot be coerced to data frame.
#'   \item Stops if 'df' has zero rows.
#'   \item Removes geometry column if the input data frame is an "sf" object.
#'   \item Removes non-numeric columns with as many unique values as rows df has.
#'   \item Converts logical columns to numeric.
#'   \item Converts factor and ordered columns to character.
#'   \item Tags the data frame with the attribute `validated = TRUE` to let the package functions skip the data validation.
#' }
#'
#'
#' @inheritParams collinear
#' @return data frame
#' @examples
#'
#' data(vi)
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #tagged as validated
#' attributes(vi)$validated
#' @autoglobal
#' @family data_validation
#' @export
validate_df <- function(
    df = NULL,
    quiet = FALSE
){

  #handle df = NULL
  if(is.null(df)){
    stop(
      "collinear::validate_df(): argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  #if already validated, return it
  df.validated <- attributes(df)$validated
  if(!is.null(df.validated)){
    if(df.validated == TRUE){
      return(df)
    }
  }

  #handle coercion to df
  if(is.data.frame(df) == FALSE){
    df <- tryCatch(
      {
        as.data.frame(df)
      },
      error = function(e){
        stop(
          "collinear::validate_df(): argument 'df' must be data frame or coercible to data frame.",
          call. = FALSE
        )
      }
    )
  }

  #stop if no rows
  if(nrow(df) == 0){
    stop(
      "collinear::validate_df(): argument 'df' has zero rows.",
      call. = FALSE
    )
  }

  #remove geometry column from df
  df <- drop_geometry_column(
    df = df,
    quiet = quiet
  )

  # replace inf with NA ----
  n_inf <- lapply(
    X = df,
    FUN = is.infinite
  ) |>
    unlist() |>
    sum()

  if(n_inf > 0){

    if(quiet == FALSE){

      message(
        "\ncollinear::validate_df(): replacing ", n_inf, " Inf value/s with NA."
      )

    }

    is.na(df) <- do.call(
      what = cbind,
      args = lapply(
        X = df,
        FUN = is.infinite
      )
    )

  }

  # logical to numeric ----
  logical_columns <- colnames(df)[
    sapply(
      X = df,
      FUN = is.logical
    )
  ]

  if(length(logical_columns) > 0){

    if(quiet == FALSE){

      message(
        "\ncollinear::validate_df(): converting these logical column/s to numeric: \n - ",
        paste0(logical_columns, collapse = "\n - ")
      )

    }

    df <- rapply(
      object = df,
      f = as.numeric,
      classes = c(
        "logical"
      ),
      how = "replace"
    )

  }

  attr(
    x = df,
    which = "validated"
  ) <- TRUE

  df

}

#' Validate Argument predictors
#'
#' @description
#' Internal function to validate the `predictors` argument. Requires the argument 'df' to be validated with [validate_df()].
#'
#' Validates the 'predictors' argument to ensure it complies with the requirements of the package functions. It performs the following actions:
#' \itemize{
#'   \item Stops if 'df' is NULL.
#'   \item Stops if 'df' is not validated.
#'   \item If 'predictors' is NULL, uses column names of 'df' as 'predictors' in the 'df' data frame.
#'   \item Print a message if there are names in 'predictors' not in the column names of 'df', and returns only the ones in 'df'.
#'   \item Stop if the number of numeric columns in 'predictors' is smaller than 'min_numerics'.
#'   \item Print a message if there are zero-variance columns in 'predictors' and returns a new 'predictors' argument without them.
#'   \item Tags the vector with the attribute `validated = TRUE` to let the package functions skip the data validation.
#' }
#'
#' @inheritParams collinear
#'
#' @return character vector: predictor names
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #validating example predictors
#' vi_predictors <- validate_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' #tagged as validated
#' attributes(vi_predictors)$validated
#'
#' @autoglobal
#' @family data_validation
#' @export
validate_predictors <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    quiet = FALSE
){


  # df ----
  df <- validate_df(
    df = df,
    quiet = quiet
  )

  # predictors ----

  #if predictors is NULL, use colnames(df)
  if(is.null(predictors)){

    if(!is.null(response)){
      for(response.i in response){
        df[[response.i]] <- NULL
      }
    }

    predictors <- colnames(df)

  } else {

    #remove response from predictors
    if(
      !is.null(response) &&
      any(response %in% predictors)
      ){

      predictors <- setdiff(
        x = predictors,
        y = response
      )

    }

    #identify wrongly named predictors
    predictors.missing <- setdiff(
      x = predictors,
      y = colnames(df)
    )

    if(length(predictors.missing) > 0){

      if(quiet == FALSE){

        message(
          "\ncollinear::validate_predictors(): these 'predictors' are not column names of 'df' and will be ignored:\n - ",
          paste(
            predictors.missing,
            collapse = "\n - "
          )
        )

      }

      #getting predictors in df only
      predictors <- intersect(
        x = predictors,
        y = colnames(df)
      )

    }

  }


  if(nrow(df) >= 10){

    #removing zero variance predictors
    predictors.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = predictors
    )

    if(length(predictors.zero.variance) > 0){

      if(quiet == FALSE){

        message(
          "\ncollinear::validate_predictors(): these predictors have near zero variance and will be ignored:\n - ",
          paste0(
            predictors.zero.variance,
            collapse = "\n - "
          )
        )

      }

      predictors <- setdiff(
        predictors,
        predictors.zero.variance
      )

    }

    #removing constant categoricals
    predictors.constant <- predictors[
      apply(
        X = df[, predictors, drop = FALSE],
        MARGIN = 2,
        FUN = function(x){
          length(unique(x)) == 1
        }
      ) |>
        unlist()
    ]

    if(length(predictors.constant) > 0){

      if(quiet == FALSE){

        message(
          "\ncollinear::validate_predictors(): these predictors have constant values and will be ignored:\n - ",
          paste0(
            predictors.constant,
            collapse = "\n - "
          )
        )

      }

      predictors <- setdiff(
        predictors,
        predictors.constant
      )

    }

  }

  attr(
    x = predictors,
    which = "validated"
  ) <- TRUE

  predictors

}


#' Validate Argument response
#'
#' @description
#' Internal function to validate the argument `response`. Requires the argument 'df' to be validated with [validate_df()].
#'
#' @inheritParams validate_predictors
#' @return character string: response name
#' @examples
#'
#' data(
#'   vi
#' )
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #validating example predictors
#' response <- validate_response(
#'   df = vi,
#'   response = "vi_numeric"
#' )
#'
#' #tagged as validated
#' attributes(response)$validated
#'
#' @autoglobal
#' @family data_validation
#' @export
validate_response <- function(
    df = NULL,
    response = NULL,
    quiet = FALSE
){

  if(is.null(response)){
    return(response)
  }

  #if already validated, return it
  if(!is.null(attr(response, "validated"))){
    return(response)
  }

  df <- validate_df(
    df = df,
    quiet = quiet
  )

  if(length(response) > 1){
    response <- response[1]
  }

  #check that the response is in df
  if(!(response %in% colnames(df))){

    if(quiet == FALSE){

      message(
        "\ncollinear::validate_response(): argument 'response' is not a column name of 'df' and will be ignored."
      )

    }
    return(NULL)
  }

  #check that it has not near-zero variance
  if(is.numeric(df[[response]]) == TRUE){

    response.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = response
    )

    if(length(response.zero.variance) == 1){

      if(quiet == FALSE){

        message(
          "\ncollinear::validate_response(): 'response' column '",
          response,
          "' has near-zero variance and will be ignored."
        )

      }
      return(NULL)
    }

  }

  #check that it is not constant
  if(length(unique(df[[response]])) == 1){

    if(quiet == FALSE){

      message(
        "\ncollinear::validate_response(): 'response' column '",
        response,
        "' has constant values and will be ignored."
      )

    }

    return(NULL)

  }

  #chedk NA
  response.na.values <- sum(is.na(df[[response]]))

  if(response.na.values > 0){

    if(quiet == FALSE){

      message(
        "\ncollinear::validate_response(): 'response' column '",
        response,
        "' has ",
        response.na.values,
        " NA values and may cause unexpected issues."
      )

    }

  }

  attr(
    x = response,
    which = "validated"
  ) <- TRUE

  response

}


#' Validate Argument preference_order
#'
#' @description
#' Internal function to validate the argument `preference_order`.
#'
#'
#' @inheritParams collinear
#' @param preference_order_auto (required, character vector) names of the predictors in the automated preference order returned by [vif_select()] or [cor_select()]
#' @param function_name (optional, character string) Name of the function performing the check. Default: "collinear::validate_preference_order()"
#'
#' @return character vector: ranked variable names
#' @export
#' @family data_validation
#' @autoglobal
#' @examples
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #validating example data frame
#' vi <- validate_df(
#'   df = vi
#' )
#'
#' #validating example predictors
#' vi_predictors <- validate_predictors(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' #tagged as validated
#' attributes(vi_predictors)$validated
#'
#' #validate preference order
#' my_order <- c(
#'   "swi_max",
#'   "swi_min",
#'   "swi_deviance" #wrong one
#' )
#'
#' my_order <- validate_preference_order(
#'   predictors = vi_predictors,
#'   preference_order = my_order,
#'   preference_order_auto = vi_predictors
#' )
#'
#' #has my_order first
#' #excludes wrong names
#' #all other variables ordered according to preference_order_auto
#' my_order
validate_preference_order <- function(
    predictors = NULL,
    preference_order = NULL,
    preference_order_auto = NULL,
    function_name = "collinear::validate_preference_order()",
    quiet = FALSE
){

  if(is.null(preference_order_auto)){
    stop(
      function_name,
      ": argument 'preference_order_auto' cannot be NULL.",
      call. = FALSE
    )
  }

  if(is.null(preference_order)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'preference_order' is NULL, ranking predictors from lower to higher multicollinearity."
      )

    }

    return(preference_order_auto)

  }

  #check if preference_order comes from preference_order()
  if(is.data.frame(preference_order) == TRUE){

    if("predictor" %in% names(preference_order)){

      preference_order <- preference_order$predictor

    } else {
      stop(
        function_name,
        ": argument 'preference_order' must be a data frame with the column 'predictor'.",
        call. = FALSE
      )
    }
  }

  #subset preference_order in predictors
  if(!is.null(predictors)){
    preference_order <- preference_order[preference_order %in% predictors]
  }

  #if there are variables not in preference_order
  #add them in the order of preference_order.auto
  if(length(preference_order) < length(predictors)){

    not_in_preference_order <- setdiff(
      x = predictors,
      y = preference_order
    )

    preference_order <- c(
      preference_order,
      preference_order_auto[
        preference_order_auto %in% not_in_preference_order
      ]
    )

  }

  preference_order

}


#' Validate Data for VIF Analysis
#'
#' @description
#' Internal function to assess whether the input arguments `df` and `predictors` result in data dimensions suitable for a VIF analysis.
#'
#' If the number of rows in `df` is smaller than 10 times the length of `predictors`, the function either issues a message and restricts `predictors` to a manageable number, or returns an error.
#'
#'
#' @inheritParams collinear
#' @param function_name (optional, character string) Name of the function performing the check. Default: "collinear::validate_data_vif()"
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
validate_data_vif <- function(
    df = NULL,
    predictors = NULL,
    function_name = "collinear::validate_data_vif()",
    quiet = FALSE
){

  df <- validate_df(
    df = df,
    quiet = quiet
  )

  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    quiet = quiet
  )

  predictors_arg <- predictors

  predictors <- identify_predictors_numeric(
    df = df,
    predictors = predictors
  )


  #minimum number of required rows
  min.rows <- length(predictors) * 10

  #manageable number of predictors with rows in df
  min.predictors <- floor(nrow(df)/10)

  #if fewer rows than required
  if(nrow(df) <= min.rows){

    if(min.predictors > 1){

      #restrict predictors to a manageable number
      predictors <- predictors[1:min.predictors]

      warning(
        function_name,
        ": VIF computation requires >=10 rows in 'df' per predictor. VIF analysis will be performed for these predictors: '",
        paste(predictors, collapse = "', '"),
        "'.",
        call. = FALSE
      )

      return(predictors)

    } else {

      stop(
        function_name,
        ": at least 10 rows per predictor are required for VIF-based multicollinearity filtering.",
        call. = FALSE
      )

    }

  }

  if(length(predictors) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": no numeric predictors available, skipping VIF-based filtering."
      )

    }

    return(character())
  }

  if(length(predictors) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one predictor available, skipping VIF-based filtering."
      )

    }

    return(predictors)
  }

  if(length(predictors) < length(predictors_arg)){

    predictors_lost <- setdiff(
      x = predictors_arg,
      y = predictors
    )

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": these predictors are not numeric and will be ignored: \n - ",
        paste(predictors_lost, collapse = "\n - "),
        "."
      )

    }

  }

  predictors

}


#' Validate Data for Correlation Analysis
#'
#' @description
#' Internal function to assess whether the input arguments `df` and `predictors` result in data dimensions suitable for pairwise correlation analysis.
#'
#' If the number of rows in `df` is smaller than 10, an error is issued.
#'
#' @inheritParams collinear
#' @param function_name (optional, character string) Name of the function performing the check. Default: "collinear::validate_data_cor()"
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
validate_data_cor <- function(
    df = NULL,
    predictors = NULL,
    function_name = "collinear::validate_data_cor()",
    quiet = FALSE
){

  df <- validate_df(
    df = df,
    quiet = quiet
  )

  if(nrow(df) < 10) {

    stop(
      function_name,
      ": at least 10 rows per predictor are required, skipping pairwise correlation filtering.",
      call. = FALSE
    )

  }

  predictors <- validate_predictors(
    df = df,
    predictors = predictors,
    quiet = quiet
  )

  if(length(predictors) == 0){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": no predictors available, skipping pairwise correlation filtering."
      )

    }
    return(predictors)
  }

  if(length(predictors) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one predictor available, skipping pairwise correlation filtering."
      )

    }
    return(predictors)
  }

  predictors

}


#' Validates Arguments of `target_encoding_lab()`
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
#' validate_encoding_arguments(
#'   df = vi,
#'   response = "vi_numeric",
#'   predictors = vi_predictors
#'   )
validate_encoding_arguments <- function(
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

  # early stops ----

  ## methods is NULL ----
  if(is.null(methods)){

    if(quiet == FALSE){

      message(
        "\ncollinear::target_encoding_lab(): argument 'encoding_method' is NULL, skipping target encoding."
      )

    }

    return(
      list(
        df = df
      )
    )

  }

  ## df ----
  df <- validate_df(
    df = df,
    quiet = quiet
  )

  ## response ----
  response <- validate_response(
    df = df,
    response = response,
    quiet = quiet
  )

  if(is.null(response)){

    if(quiet == FALSE){

      message("\ncollinear::target_encoding_lab(): argument 'response' is NULL, skipping target-encoding.")

    }

    return(
      list(
        response = NULL
      )
    )

  }

  if(!is.numeric(df[[response]])){

    if(quiet == FALSE){

      message("\ncollinear::target_encoding_lab(): argument 'response' is not numeric, skipping target-encoding.")

    }

    return(df)

  }

  ## predictors ----
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
      message("\ncollinear::target_encoding_lab(): argument 'predictors' is empty, skipping target encoding.")
    }

    return(
      list(
        predictors = predictors
      )
    )

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
        "\ncollinear::target_encoding_lab(): argument 'methods' not valid, resetting it to: '",
        paste(valid_methods, collapse = "', '"),
        "'."
      )

    }

    methods <- valid_methods

  }

  # overwrite ----
  if(is.logical(overwrite) == FALSE){

    if(quiet == FALSE){
      message("\ncollinear::target_encoding_lab(): argument 'overwrite' must be logical, resetting it to FALSE.")
    }

    overwrite <- FALSE

  }

  if(overwrite == TRUE){

    if(length(methods) > 1){

      if(quiet == FALSE){

        message(
          "\ncollinear::target_encoding_lab(): only one encoding method allowed when 'overwrite = TRUE', using method: '",
          methods[1], "'."
        )

      }

      methods <- methods[1]

    }

    if(length(white_noise) > 1){

      if(quiet == FALSE){

        message("\ncollinear::target_encoding_lab(): only one 'white_noise' value allowed when 'overwrite = TRUE', using value: ", white_noise[1], ".")

      }

      white_noise <- white_noise[1]

    }

    if(length(smoothing) > 1){

      if(quiet == FALSE){

        message("\ncollinear::target_encoding_lab(): only one 'smoothing' value allowed when 'overwrite = TRUE', using value: ", smoothing[1], ".")

      }

      smoothing <- smoothing[1]

    }

    if(length(seed) > 1){

      if(quiet == FALSE){

        message("\ncollinear::target_encoding_lab(): only one 'seed' value allowed when 'overwrite = TRUE', using value: ", seed[1], ".")

      }

      seed <- seed[1]

    }

  }

  list(
    df = df,
    response = response,
    predictors = predictors,
    methods = methods,
    smoothing = smoothing,
    white_noise = white_noise,
    seed = seed,
    overwrite = overwrite
  )


}
