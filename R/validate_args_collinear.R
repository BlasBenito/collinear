#' Validates Arguments of \code{collinear()}
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_f
#'
#' @returns list
#' @export
#' @autoglobal
validate_args_collinear <- function(
    df = NULL,
    response = NULL,
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
    response = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  ## response ----
  responses <- response
  if(!is.null(response)){

    responses <- intersect(
      x = colnames(df),
      y = response
    )

    responses_missing <- setdiff(
      x = response,
      y = responses
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

  ## f ----
  if(!is.null(f)){

    f <- validate_arg_f(
      f = f,
      f_name = f_name,
      function_name = function_name
    )

  }

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
          ": argument 'preference_order' does not match the column names in 'df' and will be ignored."
        )
      }

      preference_order <- NULL

    }

  }

  #other structures
  if(!isTRUE(attr(x = preference_order, which = "validated"))){

    #no response
    if(is.null(response)){

      if(
        is.data.frame(preference_order) ||
        is.list(preference_order)
      ){

        if(quiet == FALSE){

          message(
            "\n",
            function_name,
            ": argument 'preference_order' must be a character vector of column names in 'df' or NULL when 'response' is NULL."
          )

        }

        preference_order <- NULL

      }


    } else {

      if(is.data.frame(preference_order)){

        if(
          !all(c(
            "predictor",
            "response",
            "preference",
            "f"
          ) %in% colnames(preference_order))
        ){

          if(quiet == FALSE){

            message(
              "\n",
              function_name,
              ": dataframe 'preference_order' does not have any of the columns 'predictor', 'response', 'preference' or 'f' and will be ignored."
            )

          }

          preference_order <- NULL

        }

        preference_order <- preference_order[preference_order$response %in% response, ]

        if(nrow(preference_order) == 0){

          if(quiet == FALSE){
            message(
              "\n",
              function_name,
              ": column 'response' of the dataframe 'preference_order' does not match the values of the argument 'response' and will be ignored."
            )
          }

          preference_order <- NULL

        }


      }

      if(is.list(preference_order)){

        preference_order <- preference_order[names(preference_order %in% response)]

        if(length(preference_order) == 0){

          if(quiet == FALSE){

            message(
              "\n",
              function_name,
              ": list 'preference_order' has no elements named after the argument 'response' and will be ignored."
            )

          }

          preference_order <- NULL

        }


      }

    }

  }

  if(!is.null(preference_order)){

    attr(
      x = preference_order,
      which = "validated"
    ) <- TRUE

  }


  #store args list for output
  args <- list(
    df = df,
    response = response,
    predictors = predictors,
    encoding_method = encoding_method,
    preference_order = preference_order,
    f = attributes(f)$name,
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
      is.null(args$response) &&
      predictors_categorical_n > 0)
    {

      message(
        "\n",
        function_name,
        ": argument 'response' is NULL, skipping target encoding."
      )

    }

    if(
      !is.null(args$response) &&
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
