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
    df = NULL
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
          "collinear::validate_df(): Argument 'df' must be a data frame.",
          call. = FALSE
        )
      }
    )
  }

  #stop if no rows
  if(nrow(df) == 0){
    stop(
      "collinear::validate_df(): Argument 'df' has zero rows.",
      call. = FALSE
    )
  }

  #remove geometry column from df
  df <- drop_geometry_column(df = df)

  #replace inf with NA
  n_inf <- lapply(
    X = df,
    FUN = is.infinite
  ) |>
    unlist() |>
    sum()

  if(n_inf > 0){
    is.na(df) <- do.call(
      what = cbind,
      args = lapply(
        X = df,
        FUN = is.infinite
      )
    )
  }


  #remove non-numeric columns with as many values as rows
  non.numeric.columns <- identify_predictors_categorical(df)

  if(length(non.numeric.columns) > 0){

    non.numeric.columns.unique.values <- lapply(
      X = non.numeric.columns,
      FUN = function(x) length(unique(df[[x]]))
    ) |>
      unlist()

    names(non.numeric.columns.unique.values) <- non.numeric.columns

    columns.to.remove <- names(
      non.numeric.columns.unique.values[
        non.numeric.columns.unique.values == nrow(df)
      ]
    )

    if(length(columns.to.remove) > 0){
      message(
        "The column/s ",
        paste0(columns.to.remove, collapse = ", "),
        " have as many unique values as rows in 'df' and will be ignored."
      )
    }

    df <- df[, !(colnames(df) %in% columns.to.remove), drop = FALSE]

  }

  #convert types

  #logical to numeric
  df <- rapply(
    object = df,
    f = as.numeric,
    classes = c(
      "logical"
    ),
    how = "replace"
  )

  #factors and ordered to characters
  df <- rapply(
    object = df,
    f = as.character,
    classes = c(
      "factor",
      "ordered"
    ),
    how = "replace"
  )

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
    predictors = NULL
){


  # df ----
  if(is.null(df)){
    stop(
      "collinear::validate_predictors(): argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  if(is.null(attr(df, "validated"))){
    stop(
      "collinear::validate_predictors(): argument 'df' must be validated with collinear::validate_df().",
      call. = FALSE
    )
  }

  # predictors ----

  #if predictors is NULL, use colnames(df)
  if(is.null(predictors)){

    if(!is.null(response)){
      df[[response]] <- NULL
    }

    predictors <- colnames(df)

  } else {

    #identify wrongly named predictors
    predictors.missing <- setdiff(
      x = predictors,
      y = colnames(df)
    )

    if(length(predictors.missing) > 0){

      message(
        "collinear::validate_predictors(): these predictors are not column names of 'df' and will be ignored:\n - ",
        paste(
          predictors.missing,
          collapse = "\n - "
        )
      )

      #getting predictors in df only
      predictors <- intersect(
        x = predictors,
        y = colnames(df)
      )

    }

  }

  #removing zero variance predictors
  if(nrow(df) >= 10){

    predictors.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = predictors
    )

    if(length(predictors.zero.variance) > 0){

      message(
        "collinear::validate_predictors(): these predictors have near zero variance and will be ignored:\n - ",
        paste0(
          predictors.zero.variance,
          collapse = "\n - "
        )
      )

      predictors <- setdiff(
        predictors,
        predictors.zero.variance
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
#'   response = "vi_mean"
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
    response = NULL
){

  if(is.null(df)){
    stop(
      "collinear::validate_response(): argument 'df' cannot be NULL.",
      call. = FALSE
    )
  }

  #if not validated, stop
  if(is.null(attr(df, "validated"))){
    stop(
      "collinear::validate_response(): argument 'df' must be validated with collinear::validate_df().",
      call. = FALSE
    )
  }

  if(is.null(response)){
    return(NULL)
  }

  #if already validated, return it
  if(!is.null(attr(response, "validated"))){
    return(response)
  }

  if(is.character(response) == FALSE){
    stop(
      "collinear::validate_response(): Argument 'response' must be a character string",
      call. = FALSE
    )
  }

  if(length(response) > 1){
    response <- response[1]
  }

  #check that the response is in df
  if(!(response %in% colnames(df))){
    message(
      "collinear::validate_response(): argument 'response' with value '",
      response,
      "' is not a column name of 'df' and will be ignored."
    )
    return(NULL)
  }

  if(is.numeric(df[[response]]) == TRUE){

    response.zero.variance <- identify_predictors_zero_variance(
      df = df,
      predictors = response
    )

    if(length(response.zero.variance) == 1){
      message(
        "collinear::validate_response(): 'response' column '",
        response,
        "' has near-zero variance and will be ignored."
      )
      return(NULL)
    }

  }

  response.na.values <- sum(is.na(df[[response]]))

  if(response.na.values > 0){

    message(
      "collinear::validate_response(): 'response' column '",
      response,
      "' has ",
      response.na.values,
      " NA values and may cause unexpected issues."
    )

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
    preference_order_auto = NULL
){

  if(is.null(preference_order_auto)){
    stop("Argument 'preference_order_auto' cannot be NULL.")
  }

  if(is.null(preference_order)){
    preference_order <- preference_order_auto
  }

  #check if preference_order comes from preference_order()
  if(is.data.frame(preference_order) == TRUE){
    if("predictor" %in% names(preference_order)){
      preference_order <- preference_order$predictor
    } else {
      stop("Argument 'preference_order' must be a data frame with the column 'predictor'.")
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
#' @param function_name (optional, character string) Name of the function performing the check. Default: "collinear::vif_df()"
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
validate_data_vif <- function(
    df = NULL,
    predictors = NULL,
    function_name = "collinear::vif_df()"
){

  #minimum number of required rows
  min.rows <- length(predictors) * 10

  #manageable number of predictors with rows in df
  min.predictors <- floor(nrow(df)/10)

  #if fewer rows than required
  if(nrow(df) <= min.rows){

    if(min.predictors > 1){

      #restrict predictors to a manageable number
      predictors <- predictors[1:min.predictors]

      message(
        function_name,
        ": reliable VIF computation requires >=10 rows per predictor in 'df'. Analysis restricted to these predictors: '",
        paste(predictors, collapse = "', '"),
        "'."
      )

    } else {

      stop(
        function_name,
        ": At least 10 rows per predictor are required. VIF computation is not feasible with this data size.",
        call. = FALSE
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
#' @param function_name (optional, character string) Name of the function performing the check. Default: "collinear::cor_df()"
#'
#' @return character vector: predictors names
#' @export
#' @family data_validation
#' @autoglobal
validate_data_cor <- function(
    df = NULL,
    predictors = NULL,
    function_name = "collinear::cor_df()"
){

  if(nrow(df) < 10) {

    stop(
      function_name,
      ": At least 10 rows are required to compute reliable pairwise correlations. Analysis is not feasible with this data size.",
      call. = FALSE
    )

  }

  predictors

}


