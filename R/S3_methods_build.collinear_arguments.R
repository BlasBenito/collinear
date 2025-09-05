#' Validates Arguments of \code{collinear()}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_f
#'
#' @returns list
#' @export
#' @autoglobal
build.collinear_arguments <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    encoding_method = NULL,
    preference_order = NULL,
    f = NULL,
    f_name = NULL,
    max_cor = 0.75,
    max_vif = 5,
    quiet = FALSE,
    function_name = NULL
){

  ## max_cor max_vif ----
  if(all(is.null(c(max_cor, max_vif)))){
    stop(
      function_name,
      ": arguments 'max_cor' and 'max_vif' cannot be NULL at once.",
      call. = FALSE
    )
  }

  if(!is.null(max_cor)){
    max_cor <- validate_arg_max_cor(
      max_cor = max_cor,
      function_name = function_name,
      quiet = quiet
    )
  }

  if(!is.null(max_vif)){
    max_vif <- validate_arg_max_vif(
      max_vif = max_vif,
      function_name = function_name,
      quiet = quiet
    )
  }

  ## quiet ----
  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  ## df ----
  df <- validate_arg_df(
    df = df,
    responses = responses,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## response ----
  if(!is.null(responses)){

    responses_original <- responses

    responses <- intersect(
      x = colnames(df),
      y = responses_original
    )

    responses_missing <- setdiff(
      x = responses,
      y = responses_original
    )

    if(length(responses_missing) > 0 && quiet == FALSE){

      message(
        "\n",
        function_name,
        ": the following values of the argument 'responses' are not column names of 'df' and will be ignored: \n - ",
        paste(responses_missing, collapse = "\n - ")
      )

    }

  }

  ## encoding_method ----
  encoding_method <- validate_arg_encoding_method(
    encoding_method = encoding_method,
    overwrite = TRUE,
    function_name = function_name,
    quiet = quiet
  )

  ## predictors ----
  predictors <- validate_arg_predictors(
    df = df,
    response = NULL,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## preference_order ----
  if(is.character(preference_order)){

    preference_order <- intersect(
      x = preference_order,
      y = colnames(df)
    )

    if(length(preference_order) == 0){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": character vector 'preference_order' does not contain any column names in 'df' and  will be ignored."
        )

      }

      preference_order <- NULL

    }

  } else if(is.data.frame(preference_order)){

    if(is.null(responses)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'preference_order' cannot be of class 'dataframe' or 'list' when 'responses' is NULL and will be ignored."
        )

      }

      preference_order <- NULL

    } else if(length(responses) == 1){

      if(
        !"response" %in% colnames(preference_order) ||
        (
          "response" %in% colnames(preference_order) &&
          any(!responses %in% preference_order$response)
        )
      ){

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": column 'response' of the dataframe 'preference_order' is absent or does not match the values in argument 'responses' and will be ignored."
          )

        }

        preference_order <- NULL

      }

    } else {

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'preference_order' must be a character vector or a named list when 'responses' has more than one element, it will be ignored."
        )

      }

      preference_order <- NULL

    }

  } else if(is.list(preference_order)){

    if(is.null(responses)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": argument 'preference_order' cannot be of class 'dataframe' or 'list' when 'responses' is NULL and will be ignored."
        )

      }

      preference_order <- NULL

    } else if(any(responses %in% names(preference_order))){

      preference_order <- preference_order[responses]


    } else {

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": list 'preference_order' does not contain any element named after the values in 'responses' and will be ignored."
        )

      }

      preference_order <- NULL

    }

  } else if(!is.null(preference_order)){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": argument 'preference_order' is not valid and will be ignored."
      )

    }

    preference_order <- NULL

  }

  ## f ----
  if(!is.null(preference_order) && !is.null(f)){


    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": resetting 'f' to NULL (overridden by 'preference_order')."
      )

    }


    f <- NULL

  }

  f <- validate_arg_f(
    f = f,
    f_name = f_name,
    function_name = function_name
  )

  f_name <- attributes(f)$name

  #store args list for output
  args <- list(
    df = df,
    responses = responses,
    predictors = predictors,
    encoding_method = encoding_method,
    preference_order = preference_order,
    f = f,
    f_name = f_name,
    max_cor = max_cor,
    max_vif = max_vif,
    timestamp = Sys.time(),
    quiet = quiet
  )

  ## avoid repeating messages in loop ----
  if(args$quiet == FALSE){

    ### target encoding ----
    predictors_categorical_n <- identify_predictors_categorical(
      df = args$df,
      predictors = args$predictors,
      quiet = args$quiet
    ) |>
      length()

    if(
      !is.null(args$encoding_method) &&
      is.null(args$responses) &&
      predictors_categorical_n > 0)
    {

      message(
        "\n",
        function_name,
        ": argument 'responses' is NULL, skipping target encoding."
      )

    }

    if(
      !is.null(args$responses) &&
      is.null(args$f) &&
      is.null(args$preference_order)
    ){

      message(
        "\n",
        function_name,
        ": argument 'f' is NULL, skipping computation of preference order."
      )

    }


    if(is.null(args$max_cor)){

      message(
        "\n",
        function_name,
        ": argument 'max_cor' is NULL, skipping correlation filtering."
      )

    }

    if(is.null(args$max_vif)){

      message(
        "\n",
        function_name,
        ": argument 'max_vif' is NULL, skipping skipping VIF filtering."
      )

    }

  }

  class(args) <- c(
    class(args),
    "collinear_arguments"
  )

  args

}
